-module(hello).

-export([hello_world/0]).

%% The quintessential "Hello, World!" example.
hello_world() -> io:fwrite("hello, world~n").
