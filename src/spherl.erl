-module(spherl).
-compile([{parse_transform, lager_transform}]).
-export([start/0]).
-define(APP, spherl).
start() ->
    start(deps(?APP), [?APP]),
    case start_app(?APP) of
        ok ->
            lager:set_loglevel(lager_console_backend, info),
            ok;
        Err ->
            lager:set_loglevel(lager_console_backend, critical),
            format_error(Err),
            init:stop(1)
    end.

deps(App) ->
    case application:load(?APP) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok
    end,
    case application:get_key(App, applications) of
        {ok, Deps} ->
            Deps;
        undefined ->
            []
    end.

start([App | Rest], Seen) ->
    case lists:member(App, Seen) of
        true ->
            start(Rest, Seen);
        false ->
            Seen1 = start(deps(App), [App | Seen]),
            ok = start_app(App),
            start(Rest, Seen1)
    end;
start([], Seen) ->
    Seen.

start_app(App) ->
    case application:start(App) of
        {error, {already_started, _}} ->
            ok;
        ok when App =:= lager ->
            lager:set_loglevel(lager_console_backend, critical),
            ok;
        ok ->
            ok
    end.

format_error({error, {{eaddrinuse, Reason}, _Where}}) ->
    lager:critical("FAILURE ~p failed to listen on ~s:~p",
                   [proplists:get_value(name, Reason),
                    inet_parse:ntoa(proplists:get_value(ip, Reason)),
                    proplists:get_value(port, Reason)]),
    ok;
format_error(Err) ->
    lager:critical("~p", [Err]).
