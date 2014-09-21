-module(pio).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, high/1, low/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  {ok, Pid} = gen_server:start_link({global, ?SERVER}, ?MODULE, [], []),

  %erlang:register(pio, Pid),
  %gproc:reg({n,l,pio}, Pid),

  {ok, Pid}.

high(Pin) ->
  gen_server:call({global, ?SERVER}, {high, Pin}).

low(Pin) ->
  gen_server:call({global, ?SERVER}, {low, Pin}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({high, Pin}, _From, State) ->
    gpio:write(Pin, 1),
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

