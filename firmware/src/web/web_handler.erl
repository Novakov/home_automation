-module(web_handler).
-author("Novakov").

%% API
-export([handle/3]).

-import(respond, [text/1]).

handle(Req, 'GET', []) ->
  text("Hello World");

handle(_,_,_) -> none.