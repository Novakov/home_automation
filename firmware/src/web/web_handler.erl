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

handle(Req, 'GET', ["vm", "status"]) ->
  {Total, Allocated,_} = memsup:get_memory_data(),
  Disks = disksup:get_disk_data(),
  Applications = application:which_applications(),

  Status = [
    {memory,
      [
        {total, Total},
        {allocated, Allocated}
      ]
    },
    {disks,
      [
        [
          {id, list_to_binary(Id)},
          {size, Size},
          {used, Used}
        ]
        || {Id, Size, Used} <- Disks
      ]
    },
    {applications,
      [
        [
          {name, Name},
          {description, list_to_binary(Description)},
          {version, list_to_binary(Version)}
        ]
        || {Name, Description, Version} <- Applications
      ]
    }
  ],

  json(Status);

handle(_,_,_) -> none.