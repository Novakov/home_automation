%%%-------------------------------------------------------------------
%%% @author Novakov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. wrz 2014 14:09
%%%-------------------------------------------------------------------
-module(web_sup).
-author("Novakov").

-behaviour(supervisor).

%% API
-export([start_link/0, public_dir/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  WebConfig = [
    {ip, {0,0,0,0}},
    {port, 8088},
    {docroot, public_dir()}
  ],
  {ok,
    {
      {one_for_one, 10, 10},
      [
        {
          web_server,
          {web_dispatcher, start_link, [WebConfig]},
          permanent, 5000, supervisor, [web_dispatcher]
        }

      ]
    }
  }.

public_dir() ->
  {ok, AppName} = application:get_application(),
  Priv = case code:priv_dir(AppName) of
           {error, _} -> "priv";
           X -> X
         end,
  Priv ++ "/public".

%%%===================================================================
%%% Internal functions
%%%===================================================================
