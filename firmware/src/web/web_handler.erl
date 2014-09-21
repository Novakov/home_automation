-module(web_handler).
-author("Novakov").

%% API
-export([handle/3]).

-import(respond, [text/1, file/1, json/1]).

handle(Req, 'GET', []) ->
  respond:file("index.html");

handle(Req, 'GET', ["green"]) ->
  Status = hw_interface:green_led(),
  json([{status, Status}]);

handle(Req, 'POST', ["green", "on"]) ->
  hw_interface:green_led(on),
  text("OK");

handle(Req, 'POST', ["green", "off"]) ->
  hw_interface:green_led(off),
  text("OK");

handle(_,_,_) -> none.