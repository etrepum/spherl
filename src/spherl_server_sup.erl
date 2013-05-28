%% simple_one_for_one supervisor for spherl_server processes
-module(spherl_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

start_child(Name) ->
    supervisor:start_child(?MODULE, [[{name, binary_to_list(Name)}]]).

stop_child(Name) ->
    case spherl_server:where(Name) of
        undefined ->
            ok;
        Pid ->
            supervisor:terminate_child(?MODULE, Pid)
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
     {{simple_one_for_one, 5, 10},
      [{simple_one_for_one_child_id,
        {spherl_server, start_link, []},
        temporary,
        5000,
        worker,
        [spherl_server]}]}}.
