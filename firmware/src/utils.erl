-module(utils).

%% API
-export([record_as_json/2, rewrite_datetime_in_proplist/1]).

record_as_json(Record, Fields) ->
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