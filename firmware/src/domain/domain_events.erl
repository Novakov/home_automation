-module(domain_events).

%% API
-export([define_single_event/3, define_recurring_event/6, save_event/1, get_events_for_date_range/2]).

-include("events.hrl").

-record(singleEventDef, {start_at, end_at}).
-record(recurringEventDef, {from_date, to_date, time_start, time_end, weekDays}).

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

define_recurring_event(Target, _From, _To, _Days, _TimeStart, _TimeEnd) ->
  #event{
    target = Target,
    type = recurrring
  }.

save_event(Event) ->
  SQL = "INSERT INTO events(target, applicable_from, applicable_to, type) values(?,?,?, ?)",

  Result = emysql:execute(db, SQL, [Event#event.target, Event#event.applicable_from, Event#event.applicable_to, Event#event.type]),

  Id = emysql:insert_id(Result),

  case Event#event.type of
    single ->
      DefinitionSql = "INSERT INTO single_event_definition(event_id, start_at, end_at) values(?, ?, ?)",
      DefinitionArgs = [Id, Event#event.definition#singleEventDef.start_at, Event#event.definition#singleEventDef.end_at]
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

  SingleEvents.



read_events(ResultPacket) ->
 [
    begin
      Event = utils:emysql_row_as_record(ResultPacket, Row, event, record_info(fields, event)),

      EventDef = case Event#event.type of
        <<"single">> -> utils:emysql_row_as_record(ResultPacket, Row, singleEventDef, record_info(fields, singleEventDef))
      end,

      Event#event{definition = EventDef, type = binary_to_atom(Event#event.type, utf8)}
    end
    || Row <- ResultPacket#result_packet.rows
  ].

