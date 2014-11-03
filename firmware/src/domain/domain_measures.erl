-module(domain_measures).
-author("Novakov").

-include_lib("emysql/include/emysql.hrl").

%% API
-export([record_temperature/2, get_temperatures_between_dates/2]).

record_temperature(Timestamp, Temperature) ->
  SQL = "INSERT INTO temperatures(timestamp, temperature) VALUES(?, ?)",

  emysql:execute(db, SQL, [Timestamp, Temperature]),

  ok.

get_temperatures_between_dates(From, To) ->
  SQL = "SELECT timestamp, temperature FROM temperatures  WHERE timestamp BETWEEN ? AND ?",

  Result = emysql:execute(db, SQL, [From, To]),

  [
    [
      {timestamp, utils:emysql_decode(H)},
      {temperature, T}
    ]
  || [H,T] <- Result#result_packet.rows].