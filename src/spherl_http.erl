%% Websocket and HTTP static file service for spherl, port 8880 by default
-module(spherl_http).
-compile([{parse_transform, lager_transform}]).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3,
         websocket_terminate/3]).
-export([dispatch/0, update_dispatch/0]).

-define(APP, spherl).
-define(LISTENER, spherl_http_listener).
-record(state, {}).

init({_Any, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opts) ->
    self() ! initialize,
    spherl_server:gproc_subscribe(),
    {ok, cowboy_req:compact(Req), #state{}, hibernate}.

websocket_handle({text, JSON}, Req, State) ->
    handle_input(jsx:to_term(JSON), Req, State);
websocket_handle(_Any, Req, State) ->
    lager:debug("Ignored WS: ~p", [_Any]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

timestamp() ->
    timestamp(os:timestamp()).

timestamp({Mega, Sec, Micro}) ->
    (Mega * 1.0e9) + (Sec * 1.0e3) + (Micro * 1.0e-3).

websocket_info(initialize, Req, State) ->
    reply([{events, [[<<"clock">>, timestamp()],
                     [<<"device_list">>, spherl_server:devices()]]}],
          Req, State);
websocket_info({gproc_ps_event, spherl_server, Event}, Req, State) ->
    handle_event(Event, Req, State);
websocket_info(_Info, Req, State) ->
    lager:debug("Ignored WS Info: ~p", [_Info]),
    {ok, Req, State, hibernate}.

handle_input([<<"connect">>, Name], Req, State) ->
    proc_lib:spawn(spherl_server_sup, start_child, [Name]),
    {ok, Req, State, hibernate};
handle_input([<<"disconnect">>, Name], Req, State) ->
    spherl_server_sup:stop_child(Name),
    {ok, Req, State, hibernate};
handle_input(Term, Req, State) ->
    lager:debug("Unknown WS: ~p", [Term]),
    {ok, Req, State, hibernate}.

handle_event(Event, Req, State) ->
    io:format("Event: ~p~n", [Event]),
    reply([{events, [Event]}], Req, State).

reply(Term, Req, State) ->
    {reply, {text, jsx:to_json(Term)}, Req, State, hibernate}.

dispatch() ->
    cowboy_router:compile(
      [{'_',
        [{<<"/">>,
          cowboy_static,
          [{directory, {priv_dir, ?APP, [<<"www">>]}},
           {file, <<"index.html">>},
           {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
         {<<"/static/[...]">>,
          cowboy_static,
          [{directory, {priv_dir, ?APP, [<<"www">>, <<"static">>]}},
           {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
         {<<"/ws">>,
          spherl_http,
          []}]}]).

update_dispatch() ->
    %% Live update of routes
    cowboy:set_env(?LISTENER, dispatch, dispatch()).
