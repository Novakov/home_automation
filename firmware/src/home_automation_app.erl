-module(home_automation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Modes} = application:get_env(modes),
    {ok, DbServer} = application:get_env(dbserver),

    error_logger:info_msg("Running with modes ~p~n", [Modes]),

    ok = emysql:add_pool(db, 1, "root", "root", DbServer, 3306, "home_automation", utf8),

    home_automation_sup:start_link(Modes).

stop(_State) ->
    ok.
