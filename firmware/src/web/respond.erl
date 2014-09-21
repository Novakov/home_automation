-module(respond).
-author("Novakov").

%% API
-export([text/1, json/1, view/2, file/1]).

text(Text) ->
  fun(R) -> R:respond({200, [], Text}) end.

json(Value) -> 	
  fun(Req) ->
    Encoded = jsx:encode(Value),
    Req:respond({200, [{"Content-Type", "application/json"}], Encoded})
  end.

view(ViewModule, Parameters) ->
  fun(Req) ->
	  {ok, Rendered} = ViewModule:render(Parameters),
	  Req:respond({200, [{"Content-Type", "text/html"}], Rendered})
  end.

file(FileName) ->
  fun(Req) ->
    Req:serve_file(FileName, "./public")
  end.