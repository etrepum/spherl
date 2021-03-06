-module(spherl_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(APP, spherl).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    TransOpts = [{Key, Value} ||
                    Key <- [port, ip],
                    {ok, Value} <- [application:get_env(?APP, Key)]],
    {ok, NumAcceptors} = application:get_env(?APP, num_acceptors),
    case spherl_sup:start_link() of
        {ok, Pid} ->
            case check_listener(
                   spherl_http_listener,
                   NumAcceptors,
                   TransOpts,
                   [{env, [{dispatch, spherl_http:dispatch()}]}]) of
                {ok, _Listener} ->
                    {ok, Pid};
                ListenErr ->
                    ListenErr
            end;
        SupErr ->
            SupErr
    end.

stop(_State) ->
    cowboy:stop_listener(spherl_http_listener),
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
