-type datetime() :: {{number(), number(), number()}, {number(), number(), number()}}.

-record(event_occurence, {series_id :: number(),
  from :: datetime(),
  to :: datetime(),
  target :: string(),
  isRecurring :: boolean()
}).