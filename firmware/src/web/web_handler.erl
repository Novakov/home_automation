-module(web_handler).
-author("Novakov").

%% API
-export([handle/3]).

-import(respond, [text/1, file/1, json/1]).

-record(test_row, {id, name}).

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
        || {Id, Size, Used} <- Disks, Size > 0, Used > 0
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

handle(_Req, 'GET', ["now"]) ->
  IsoDate = iso8601:format(now()),
  json([{date, IsoDate}]);

handle(_Req, 'GET', ["events"]) ->
  Events = [
    make_event({{2014, 09, 22}, {12,0,0}}, {{2014, 09, 22}, {14,0,0}}),
    make_event({{2014, 09, 22}, {13,30,0}}, {{2014, 09, 22}, {17,0,0}})
  ],
  json(Events);

handle(_Req, 'GET', ["db"]) ->
  Sql = "select id, name from test",
  Result = emysql:execute(db, Sql),
  Rows = emysql_util:as_record(Result, test_row, record_info(fields, test_row)),
  Json = [
    [
      {id, X#test_row.id},
      {name, X#test_row.name}
    ]
    || X <- Rows
  ],

  json(Json);

handle(_,_,_) -> none.


make_event(Start, End) ->
  [
    {title, <<"">>},
    {start, iso8601:format(Start)},
    {'end', iso8601:format(End)}
  ].