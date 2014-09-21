-module(home_automation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting!"),

    {ok, Modes} = application:get_env(modes),

    error_logger:info_msg("Running with modes ~p~n", [Modes]),

    home_automation_sup:start_link(Modes).

stop(_State) ->
    ok.
