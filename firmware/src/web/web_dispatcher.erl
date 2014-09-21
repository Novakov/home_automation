-module(web_dispatcher).

%% API
-export([start_link/1, stop/0, webSocketLoop/3]).


start_link(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),

  PipelineElements = [
    fun errorHandler/2,
    fun supportWebSockets/2,
    fun(Req, Next) -> staticFiles(Req, DocRoot, Next) end,
    fun dispatchHandler/2
  ],

  Pipeline = lists:foldr(
    fun(Func, Next) -> fun (Req) -> Func(Req, Next) end end,
    fun emptyHandler/1,
    PipelineElements
  ),

  mochiweb_http:start_link([{name, ?MODULE}, {loop, Pipeline} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

emptyHandler(Req) -> Req:not_found().

errorHandler(Req, Next) ->
  try
    Next(Req)
  catch
    E ->
      Msg = "Error " ++ atom_to_list(E) ++ " at " ++
        format_stack_trace(erlang:get_stacktrace()),
      Req:respond({500, [], Msg})
  end.

staticFiles(Req, DocRoot, Next) ->
    "/" ++ Path = Req:get(path),
  case filelib:is_regular(filename:join([DocRoot, Path])) of
    true ->
      Req:serve_file(Path, DocRoot);
    false ->
      Next(Req)
  end.

dispatchHandler(Req, Next) ->
  Path = Req:get(path),
  PathElements = lists:filter(fun(P) -> P =/= "" end, re:split(Path, "/", [notempty_atstart, {return, list}])),

  case web_handler:handle(Req, Req:get(method), PathElements) of
    none -> Next(Req);
    ResponseFunc -> ResponseFunc(Req)
  end.

supportWebSockets(Req, Next) ->
  UpgradeHeader = mochiweb_request:get_header_value("Upgrade", Req),

  if
    UpgradeHeader =/= "websocket" -> Next(Req);
    true ->
      {ReentryWS, ReplyChannel} = mochiweb_websocket:upgrade_connection(Req, fun ?MODULE:webSocketLoop/3),

      web_broadcaster:register(ReplyChannel),

      ReentryWS(null)
  end.

webSocketLoop(Payload, _State, ReplyChannel) ->
  Received = list_to_binary(Payload),
  ReplyChannel("Hello " ++ Received),

  _State.

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

format_stack_trace(StackTrace) ->
  lists:foldl(fun(Item, Acc) -> Acc ++ "\n" ++ format_stack_trace_item(Item) end, "", StackTrace).

format_stack_trace_item({Module, Function, Arity, Location}) ->
  atom_to_list(Module)
  ++ ":" ++ atom_to_list(Function) ++ "/" ++ integer_to_list(Arity)
    ++ format_location(Location).

format_location([{file, File}, {line, Line}]) ->
  " in " ++ File ++ ":" ++ integer_to_list(Line).

