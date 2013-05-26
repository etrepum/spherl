-module(spherl_server).
-behaviour(gen_server).
-export([all/0, where/1, devices/0]).
-export([send/2, send_async/2, set_async_process/2]).
-export([start/1, start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-define(DEFAULT_NAME, "tty.Sphero-PYO-RN-SPP").

-record(state, { name = ?DEFAULT_NAME
               , server_opts = []
               , retries = 10
               , dev = undefined
               , uart = undefined
               , buffer = <<>>
               , sequence_num = 1
               , continuations = dict:new()
               , async_process = undefined
               }).

where(Name) ->
    gproc:where({n, l, {?MODULE, Name}}).

devices() ->
    {ok, Devs} = uart_devices:get_list(),
    All = all(),
    lists:flatmap(
      fun (Dev) ->
              N = iolist_to_binary(Dev),
              case binary:match(N, <<"Sphero-">>) of
                  nomatch ->
                      [];
                  _Found ->
                      [{N, lists:keyfind(N, 1, All) =/= false}]
              end
      end,
      Devs).

all() ->
    gproc:lookup_values({p, l, {?MODULE, name}}).

send(Pid, Packet) ->
    gen_server:call(Pid, {send, Packet}).

send_async(Pid, Packet) ->
    gen_server:cast(Pid, {send_async, Packet}).

set_async_process(Pid, Proc) ->
    gen_server:cast(Pid, {set_async_process, Proc}).

start(Options) ->
    gen_start(start, Options).

start_link(Options) ->
    gen_start(start_link, Options).

gen_start(F, Options) ->
    State = parse_options(Options, #state{}),
    #state{server_opts=ServerOpts} = State,
    gen_server:F(?MODULE, State, ServerOpts).

parse_options(Options, State) ->
    Fs = [{name, #state.name},
          {server_opts, #state.server_opts},
          {retries, #state.retries}],
    lists:foldr(
      fun ({K, V}, S) ->
              case lists:keyfind(K, 1, Fs) of
                  {_, N} ->
                      setelement(N, S, V);
                  false ->
                      throw({unknown_option, K})
              end
      end,
      State,
      Options).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

init(State=#state{name=Name, retries=Retries}) ->
    case uart_devices:alloc(Name) of
        {ok, Dev} ->
            case open(Dev, Retries) of
                {ok, Uart} ->
                    gproc_register(State#state{uart=Uart, dev=Dev});
                {error, Err} ->
                    {stop, {uart, Err}}
            end;
        {error, Err} ->
            {stop, {uart, Err}}
    end.

gproc_register(State=#state{name=Name}) ->
    N = iolist_to_binary(Name),
    true = gproc:reg({n, l, {?MODULE, N}}),
    true = gproc:reg({p, l, {?MODULE, name}}, N),
    gproc_publish([<<"device_connected">>, N]),
    {ok, State}.

gproc_unregister(#state{name=Name}) ->
    N = iolist_to_binary(Name),
    gproc_publish([<<"device_disconnected">>, N]),
    ok.

gproc_publish(Event) when is_list(Event) ->
    gproc_ps:publish(l, spherl_client, Event).

handle_call({exec, F}, _From, State) ->
    {reply, F(State), State};
handle_call({send, Packet}, From, State) ->
    {noreply, add_continuation(
                From, Packet,
                send_packet(Packet, true, State))};
handle_call(Req, From, State) ->
    error_logger:format("** ~s ~s received unknown call\n"
                        "** message ~p\n"
                        "** from ~p\n",
                        [?MODULE, name(State),
                         Req,
                         From]),
    {reply, error, State}.

handle_cast({send_async, Packet}, State) ->
    {noreply, ignore_continuation(send_packet(Packet, false, State))};
handle_cast({set_async_process, Proc}, State) when (is_pid(Proc) orelse
                                                    Proc =:= undefined) ->
    {noreply, State#state{async_process=Proc}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Req, State) ->
    error_logger:format("** ~s ~s received unknown cast\n"
                        "** message ~p\n",
                        [?MODULE, name(State),
                         Req]),
    {noreply, State}.

handle_info({uart, Uart, Data},
            State=#state{uart=Uart, buffer=Buffer}) ->
    {noreply, receive_packets(<<Buffer/binary, Data/binary>>, State)};
handle_info({uart_closed, Uart}, State=#state{uart=Uart}) ->
    {stop, {error, uart_closed}, State};
handle_info(Req, State) ->
    error_logger:format("** ~s ~s received unknown message\n"
                        "** message ~p\n",
                        [?MODULE, name(State),
                         Req]),
    {noreply, State}.

name(#state{name=Name}) ->
    Name.

terminate(_Reason, State=#state{uart=Uart, name=Name}) ->
    gproc_unregister(State),
    uart:close(Uart),
    uart_devices:release(Name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

receive_packets(Data, State) ->
    case spherl_cmd:decode_packet(Data) of
        {more, _N} ->
            State#state{buffer=Data};
        {ok, Packet, Rest} ->
            receive_packets(Rest, handle_packet(Packet, State))
    end.

handle_packet({async, Packet}, State) ->
    push_async(Packet, State),
    State;
handle_packet(Packet={response, _MSRP, SEQ, _Data}, State) ->
    run_continuation(SEQ, Packet, State).

run_continuation(SEQ, Packet, State=#state{continuations=D}) ->
    case dict:find(SEQ, D) of
        {ok, C} ->
            continue(C, Packet),
            State#state{continuations=dict:erase(SEQ, D)};
        error ->
            State
    end.

ignore_continuation({_Seq, S}) ->
    S.

add_continuation(From, Packet, {Seq, S=#state{continuations=D}}) ->
    S#state{continuations=dict:store(Seq, {From, Packet}, D)}.

continue({From, Orig}, Packet) ->
    gen_server:reply(From, spherl_cmd:interpret_response(Orig, Packet)).

send_packet(Packet, Answer, State=#state{uart=Uart, sequence_num=N}) ->
    ok = uart:send(Uart, spherl_cmd:encode_packet(Packet, N, Answer, true)),
    {N, State#state{sequence_num=(N + 1) band 255}}.

push_async(Msg, #state{async_process=Pid}) when is_pid(Pid) ->
    Pid ! {?MODULE, self(), Msg};
push_async(_Msg, _State) ->
    ok.

open(Dev, Retries) when is_integer(Retries) ->
    case uart:open(Dev, [{baud, 115200},
                         {mode, binary},
                         {packet, raw},
                         {active, true},
                         {exit_on_close, true}]) of
        {error, _Err} when Retries > 0 ->
            open(Dev, Retries - 1);
        Res ->
            Res
    end.
