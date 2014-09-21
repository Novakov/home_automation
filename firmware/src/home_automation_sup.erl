-module(home_automation_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, gpio_sup_spec/0]).

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

gpio_sup_spec() ->
  GpioSpec = [
    {21, output}
  ],
  {gpio,
    {gpio_sup, start_link, [GpioSpec]},
    permanent, 5000, worker, [gpio_sup]
  }.

init([]) ->
  GpioSpec = [
    {21, output}
  ],
  {
    ok,
    {
      {one_for_all, 5, 60},
      [
        {gpio,
          {gpio_sup, start_link, [GpioSpec]},
          permanent, 5000, worker, [gpio_sup]
        },
        {pio,
          {pio, start_link, []},
          permanent, 5000, worker, [pio]
        }
      ]
    }
  }.

