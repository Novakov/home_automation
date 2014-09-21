-module(home_automation_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Modes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Modes).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

child_specs([hw|Rest]) ->
  HwInterfaceSpec = {hw_interface_sup,
           {hw_interface_sup, start_link, []},
           permanent, 5000, supervisor, [hw_interface_sup]
         },
  [HwInterfaceSpec|child_specs(Rest)];

child_specs([web|Rest]) ->
  WebSpec = {web_sup,
    {web_sup, start_link, []},
    permanent, 5000, supervisor, [web_sup]
  },
  [WebSpec|child_specs(Rest)];

child_specs([]) ->
  [].

init(Modes) ->
  error_logger:info_msg("Running with modes ~p~n", [Modes]),
  {
    ok,
    {
      {one_for_all, 5, 60},
      child_specs(Modes)
    }
  }.

