-module(web_handler).
-author("Novakov").

%% API
-export([handle/3]).

-import(respond, [text/1, file/1, json/1]).

-include("events.hrl").

handle(_Req, 'GET', []) ->
  respond:file("index.html");

handle(_Req, 'GET', ["green"]) ->
  Status = hw_interface:green_led(),
  json([{status, Status}]);

handle(_Req, 'POST', ["green", "on"]) ->
  hw_interface:green_led(on),
  text("OK");

handle(_Req, 'POST', ["green", "off"]) ->
  hw_interface:green_led(off),
  text("OK");

handle(_Req, 'GET', ["vm", "status"]) ->
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

handle(Req, 'GET', ["events"]) ->
  QS = Req:parse_qs(),

  From = iso8601:parse(proplists:get_value("from", QS)),
  To = iso8601:parse(proplists:get_value("to", QS)),

  Events = domain_events:get_events_for_date_range(From, To),

  Proplist = [ utils:record_as_proplist(X, record_info(fields, event_occurence)) || X <- Events ],

  json(utils:rewrite_datetime_in_proplist(Proplist));

handle(Req, 'POST', ["events", "new"]) ->
  Input = jsx:decode(Req:recv_body()),
  P = fun (Name) -> proplists:get_value(Name, Input) end,
  Event = domain_events:define_single_event(
    P(<<"target">>),
    calendar:universal_time_to_local_time(iso8601:parse(P(<<"from">>))),
    calendar:universal_time_to_local_time(iso8601:parse(P(<<"to">>)))
  ),

  ok = domain_events:save_event(Event),

  json([{result, <<"OK">>}]);

handle(_,_,_) -> none.