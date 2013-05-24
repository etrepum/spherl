-module(spherl).
-export([start/0]).

start() ->
    lists:foreach(fun application:start/1,
                  [dthread, fnotify, uart, crypto, ranch, cowboy]),
    application:start(spherl).
