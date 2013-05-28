%% Sends spherl_disconnect messages for spherl_server processes that
%% die unexpectedly
-module(spherl_monitor).
-behaviour(gen_server).
-export([start/0, start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-record(state, {monitors :: dict(), names :: dict()}).

start() ->
    gen_start(start).

start_link() ->
    gen_start(start_link).

gen_start(F) ->
    gen_server:F(?MODULE, #state{}, []).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

init(State) ->
    ok = spherl_server:gproc_subscribe(),
    {ok, State}.

handle_call(Req, From, State) ->
    error_logger:format("** ~s ~s received unknown call\n"
                        "** message ~p\n"
                        "** from ~p\n",
                        [?MODULE, name(State),
                         Req,
                         From]),
    {reply, error, State}.

name(_State) ->
    ?MODULE.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Req, State) ->
    error_logger:format("** ~s ~s received unknown cast\n"
                        "** message ~p\n",
                        [?MODULE, name(State),
                         Req]),
    {noreply, State}.

handle_info({gproc_ps_event, spherl_server, Event}, State) ->
    {noreply, handle_event(Event, State)};
handle_info({'DOWN', MRef, _Type, _Object, _Info}, State) ->
    {noreply, handle_down(MRef, State)};
handle_info(Req, State) ->
    error_logger:format("** ~s ~s received unknown message\n"
                        "** message ~p\n",
                        [?MODULE, name(State),
                         Req]),
    {noreply, State}.

terminate(_Reason, _State) ->
    gproc:goodbyte(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    State.

handle_down(MRef, State=#state{monitors=DM, names=DN}) ->
    case dict:find(MRef, DM) of
        {ok, Name} ->
            spherl_server:publish([<<"device_disconnected">>, Name]),
            State#state{monitors=dict:erase(MRef, DM),
                        names=dict:erase(Name, DN)};
        error ->
            State
    end.

handle_event([<<"device_connected">>, Name],
             State=#state{monitors=DM, names=DN}) ->
    case spherl_server:where(Name) of
        undefined ->
            State;
        Pid ->
            MRef = erlang:monitor(process, Pid),
            State#state{monitors=dict:store(MRef, Name, DM),
                        names=dict:store(Name, MRef, DN)}
    end;
handle_event([<<"device_disconnected">>, Name],
             State=#state{monitors=DM, names=DN}) ->
    case dict:find(Name, DN) of
        {ok, MRef} ->
            erlang:demonitor(MRef, [flush]),
            State#state{monitors=dict:erase(MRef, DM),
                        names=dict:erase(Name, DN)};
        error ->
            State
    end;
handle_event(_Other, State) ->
    State.
