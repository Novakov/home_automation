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
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Modes]).

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
  child_specs(Rest);

child_specs([]) ->
  [].

init(Modes) ->
  {
    ok,
    {
      {one_for_all, 5, 60},
      child_specs(Modes)
    }
  }.

