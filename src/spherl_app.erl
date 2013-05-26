-module(spherl_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(APP, spherl).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(?APP, port),
    {ok, NumAcceptors} = application:get_env(?APP, num_acceptors),
    first_ok([fun spherl_sup:start_link/0,
              fun () ->
                      check_listener(
                        spherl_http_listener,
                        NumAcceptors,
                        [{port, Port}],
                        [{env, {dispatch, spherl_http:dispatch()}}])
              end]).

first_ok([F | Rest]) ->
    first_ok(Rest, F()).

first_ok([F | Rest], R={ok, _}) ->
    case F() of
        {ok, _} ->
            first_ok(Rest, R);
        Err ->
            Err
    end;
first_ok(_Fs, Err) ->
    Err.

stop(_State) ->
    ok.

check_listener(Name, NumAcceptors, TransOpts, ProtoOpts) ->
    case cowboy:start_http(Name, NumAcceptors, TransOpts, ProtoOpts) of
        R={ok, _} ->
            R;
        {error, _Err} ->
            {error, {eaddrinuse,
                     [{name, Name},
                      {ip, proplists:get_value(ip, TransOpts, {0,0,0,0})},
                      {port, proplists:get_value(port, TransOpts, 0)}]}}
    end.
