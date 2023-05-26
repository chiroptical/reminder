-type color() :: red | orange | yellow | green | blue | indigo | violet.

-record(cat, {
    name :: string(),
    color = green :: color(),
    description :: string()
}).

-type cat() :: #cat{}.
