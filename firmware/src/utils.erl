-module(utils).

%% API
-export([record_as_proplist/2, rewrite_datetime_in_proplist/1, date_part/1, time_part/1, emysql_as_map/1, emysql_row_as_record/4, add_days/2, min_date/2, max_date/2]).

-include_lib("emysql/include/emysql.hrl").

record_as_proplist(Record, Fields) ->
  [
    {lists:nth(Idx, Fields), erlang:element(Idx + 1, Record)}
    || Idx <- lists:seq(1, length(Fields))
  ].

rewrite_datetime_in_proplist(Proplist) ->
  [
    begin
      case E of
        {Key, {datetime, DT}} -> {Key, DT};
        _ when is_list(E) -> rewrite_datetime_in_proplist(E);
        _ -> E
      end
    end
    || E <- Proplist
  ].

date_part({datetime, {Date, _Time}}) ->
  Date;
date_part({Date, _Time}) ->
  Date.

time_part({datetime, {_Date, Time}}) ->
  Time;
time_part({_Date, Time}) ->
  Time.

min_date(A, B) when A < B -> A;
min_date(A, B) when B < A -> B;
min_date(A, A) -> A.

max_date(A, B) when A > B -> A;
max_date(A, B) when B > A -> B;
max_date(A, A) -> A.

emysql_decode({datetime, DateTime}) -> DateTime;
emysql_decode(X) -> X.

emysql_row_as_record(Result, Row, RecordName, RecordFields) ->
  Columns = Result#result_packet.field_list,
  Indexes = lists:seq(1, length(Columns)),
  IndexedColumns = lists:zip([binary_to_atom(C#field.name, utf8) || C <- Columns], Indexes),

  RecordValues = [
    begin
      case proplists:get_value(FieldName, IndexedColumns) of
        undefined -> undefined;
        Index -> emysql_decode(lists:nth(Index, Row))
      end
    end
    || FieldName <- RecordFields
  ],

  list_to_tuple([RecordName|RecordValues]).

emysql_as_map(_Result = #result_packet{rows = Rows, field_list = Fields}) ->
  emysql_as_map(Rows, Fields, []).

emysql_as_map([], _Fields, Result)
  -> Result;
emysql_as_map([Row|Rest], Fields, Converted) ->
  ConvertedRow = row_to_map(Row, Fields, #{}),

  emysql_as_map(Rest, Fields, [ConvertedRow|Converted]).

row_to_map([],[], Map)
  -> Map;
row_to_map([Value|RemainingValues], [Field|RemainingFields], Map) ->
  row_to_map(RemainingValues, RemainingFields, maps:put(Field, Value, Map)).

add_days(Count, DateTime) ->
  Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time_to_universal_time(DateTime)),
  FutureSeconds = Seconds + Count * 24 * 3600,
  calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(FutureSeconds)).