-module(hw_interface).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, green_led/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("io_spec.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  {ok, Pid} = gen_server:start_link({global, ?SERVER}, ?MODULE, [], []),

  {ok, Pid}.

green_led(on) ->
  gen_server:call({global, ?SERVER}, {green_led, on});

green_led(off) ->
  gen_server:call({global, ?SERVER}, {green_led, off}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({green_led, LedState}, _From, State) ->
    gpio:write(?GREEN_LED_PIN, onOff(LedState)),
    {reply, ok, State};

handle_call({low, Pin}, _From, State) ->
    gpio:write(Pin, 0),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
  {reply, {unk, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

onOff(on) -> 0;
onOff(off) -> 1.

