-module(domain_measures).
-author("Novakov").

-include_lib("emysql/include/emysql.hrl").

%% API
-export([record_temperature/2]).

record_temperature(Timestamp, Temperature) ->
  SQL = "INSERT INTO temperatures(timestamp, temperature) VALUES(?, ?)",

  emysql:execute(db, SQL, [Timestamp, Temperature]),

  ok.