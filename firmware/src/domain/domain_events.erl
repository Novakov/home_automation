-module(domain_events).

%% API
-export([define_single_event/3, define_recurring_event/4, save_event/1, get_events_for_date_range/2, delete_event/1, recurring_event_occurences/4, read_events/1]).

-include("events.hrl").

-record(singleEventDef, {start_at, end_at}).
-record(recurringEventDef, {time_start, time_end, weekDays}).

-record(event, {id :: number(),
                target :: string(),
                type :: 'single' | 'recurring',
                applicable_from :: datetime(),
                applicable_to :: datetime(),
                definition :: #singleEventDef{} | #recurringEventDef{}
}).



-include_lib("emysql/include/emysql.hrl").

define_single_event(Target, From, To) ->
  #event{
    target = Target,
    type = single,
    applicable_from = utils:date_part(From),
    applicable_to = utils:date_part(To),
    definition = #singleEventDef{
      start_at = From,
      end_at = To
    }
  }.

define_recurring_event(Target, From, To, Days) ->
  #event{
    target = Target,
    type = recurring,
    applicable_from = utils:date_part(From),
    applicable_to = utils:date_part(To),
    definition = #recurringEventDef{
      time_start = From,
      time_end = To,
      weekDays = Days
    }
  }.

save_event(Event) ->
  SQL = "INSERT INTO events(target, applicable_from, applicable_to, type) values(?,?,?, ?)",

  Result = emysql:execute(db, SQL, [Event#event.target, Event#event.applicable_from, Event#event.applicable_to, Event#event.type]),

  Id = emysql:insert_id(Result),

  case Event#event.type of
    single ->
      DefinitionSql = "INSERT INTO single_event_definition(event_id, start_at, end_at) values(?, ?, ?)",
      DefinitionArgs = [
        Id,
        Event#event.definition#singleEventDef.start_at,
        Event#event.definition#singleEventDef.end_at
      ];
    recurring ->
      WeekDaysString = string:join(lists:map(fun(I)->erlang:integer_to_list(I) end, Event#event.definition#recurringEventDef.weekDays), ","),
      DefinitionSql = "INSERT INTO recurring_event_definition(event_id, time_start, time_end, reccur_days) values(?, ?, ?, ?)",
      DefinitionArgs = [
        Id,
        Event#event.definition#recurringEventDef.time_start,
        Event#event.definition#recurringEventDef.time_end,
        WeekDaysString
      ]
  end,

  emysql:execute(db, DefinitionSql, DefinitionArgs),

  ok.

get_events_for_date_range(From, To) ->
  SQL = "SELECT * FROM full_events WHERE applicable_from <= ? AND applicable_to >= ?",

  Result = emysql:execute(db, SQL, [To, From]),

  Events = read_events(Result),

  %error_logger:info_msg("Got events: ~p~n", [Events]),

  SingleEvents = [
    #event_occurence{series_id = E#event.id,
                     target = E#event.target,
                     from = E#event.definition#singleEventDef.start_at,
                     to = E#event.definition#singleEventDef.end_at
    }
    || E <- Events, E#event.type =:= single
  ],

  RecurringEvents = [
    recurring_event_occurences(E, utils:max_date(From, E#event.applicable_from), utils:min_date(To, E#event.applicable_to), [])
    || E <- Events, E#event.type =:= recurring
  ],

  SingleEvents ++ lists:flatten(RecurringEvents).

delete_event(EventId) ->
  Result = emysql:execute(db, "DELETE FROM events WHERE id = ?", [EventId]),
  1 = emysql:affected_rows(Result),
  ok.

recurring_event_occurences(Event, From, To, Acc) when From > To ->
  Acc;
recurring_event_occurences(Event, From, To, Acc) ->
  Occurence = #event_occurence{
    series_id = Event#event.id,
    target = Event#event.target,
    from = {utils:date_part(From), utils:time_part(Event#event.definition#recurringEventDef.time_start)},
    to = {utils:date_part(From), utils:time_part(Event#event.definition#recurringEventDef.time_end)}
  },
  recurring_event_occurences(Event, utils:add_days(1, From), To, [Occurence|Acc]).

read_events(ResultPacket) ->
 [
    begin
      Event = utils:emysql_row_as_record(ResultPacket, Row, event, record_info(fields, event)),

      EventDef = case Event#event.type of
        <<"single">> -> utils:emysql_row_as_record(ResultPacket, Row, singleEventDef, record_info(fields, singleEventDef));
        <<"recurring">> -> utils:emysql_row_as_record(ResultPacket, Row, recurringEventDef, record_info(fields, recurringEventDef))
      end,

      Event#event{definition = EventDef, type = binary_to_atom(Event#event.type, utf8)}
    end
    || Row <- ResultPacket#result_packet.rows
  ].

