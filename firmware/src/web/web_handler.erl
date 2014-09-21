-module(web_handler).
-author("Novakov").

%% API
-export([handle/3]).

-import(respond, [text/1]).

handle(Req, 'GET', []) ->
  text("Hello World");

handle(Req, 'GET', ["green", "on"]) ->
  hw_interface:green_led(on),
  text("OK");

handle(Req, 'GET', ["green", "off"]) ->
  hw_interface:green_led(off),
  text("OK");

handle(_,_,_) -> none.