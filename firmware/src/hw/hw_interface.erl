-module(hw_interface).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, green_led/1, green_led/0, temperature/0]).

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

green_led() ->
  case gen_server:call({global, ?SERVER}, {green_led, ask}) of
    1 -> off;
    0 -> on
  end.

green_led(on) ->
  gen_server:call({global, ?SERVER}, {green_led, on});

green_led(off) ->
  gen_server:call({global, ?SERVER}, {green_led, off}).

temperature() ->
  gen_server:call({global, ?SERVER}, {temperature, ask}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ok = gpio:set_int(?BTN_PIN, rising),
    {ok, Args}.

handle_call({green_led, ask}, _From, State) ->
  Status = gpio:read(?GREEN_LED_PIN),
  {reply, Status, State};

handle_call({green_led, LedState}, _From, State) ->
    gpio:write(?GREEN_LED_PIN, onOff(LedState)),
    {reply, ok, State};

handle_call({temperature, ask}, _From, State) ->
  {ok, Temperature} = ds18b20:read_temperature("/sys/bus/w1/devices/28-00044eff2dff/w1_slave"),
  {reply, Temperature, State};

handle_call({low, Pin}, _From, State) ->
    gpio:write(Pin, 0),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
  {reply, {unk, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gpio_interrupt,?BTN_PIN, rising}, _State) ->
  error_logger:info_msg("Rising edge!"),
  {noreply, _State};
handle_info(_Info, State) ->
  error_logger:info_msg("Unknown msg ~p!", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
  error_logger:info_msg("Terminaing hw_interface Reason ~p!!!!!~n", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

onOff(on) -> 0;
onOff(off) -> 1.

