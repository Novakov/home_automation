-module(home_automation_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {
    ok,
    {
      {one_for_all, 5, 60},
      [
        {hw_interface_sup,
          {hw_interface_sup, start_link, []},
          permanent, 5000, supervisor, [hw_interface_sup]
        }
      ]
    }
  }.

