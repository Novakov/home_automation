-module(domain_events).

%% API
-export([define_single_event/3, define_recurring_event/6, save_event/1, get_events_for_date_range/2]).

-record(event, {id, target, type, eventDef}).
-record(simpleEventDef, {from, to}).
-record(recurringEventDef, {from_date, to_date, time_start, time_end, weekDays}).

define_single_event(Target, From, To) ->
  #event{
    target = Target,
    type = target,
    eventDef = #simpleEventDef{from=From, to=To}
  }.

define_recurring_event(Target, From, To, Days, TimeStart, TimeEnd) ->
  #event{
    target = Target,
    type = recurrring,
    eventDef = #recurringEventDef{
      from_date = From,
      to_date = To,
      time_start = TimeStart,
      time_end = TimeEnd,
      weekDays = Days
    }
  }.

save_event(Event) ->
  ok = emysql:prepare(insert_event, "INSERT INTO events(target, `from`, `to`) values(?,?,?)"),
  Result = emysql:execute(db, insert_event, [Event#event.type, Event#event.eventDef#simpleEventDef.from, Event#event.eventDef#simpleEventDef.to]),

  error_logger:info_msg("Result: ~p~n", [Result]),

  ok.

get_events_for_date_range(From, To) ->
  SQL = "SELECT * FROM events WHERE `from` <= ? AND `to` >= ?",
  ok = emysql:prepare(select_event_for_range, SQL),
  Result = emysql:execute(db, select_event_for_range, [To, From]),

  emysql_util:as_proplist(Result).

