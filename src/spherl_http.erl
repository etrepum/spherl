-module(spherl_http).
-compile([{parse_transform, lager_transform}]).
-define(APP, spherl).
-define(LISTENER, spherl_http_listener).

-export([dispatch/0, update_dispatch/0]).
dispatch() ->
    cowboy_router:compile(
      [{'_',
        [{"/[...]",
          cowboy_static,
          [{directory, {priv_dir, ?APP, [<<"static">>]}},
           {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}]}]).

update_dispatch() ->
    %% Live update of routes
    cowboy:set_env(?LISTENER, dispatch, dispatch()).
