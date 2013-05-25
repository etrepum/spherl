-module(spherl_server).
-behaviour(gen_server).
-export([send/2, send_async/2]).
-export([start/1, start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-define(DEFAULT_DEV, "/dev/tty.Sphero-PYO-RN-SPP").

-record(state, { name = undefined
               , server_opts = []
               , dev = ?DEFAULT_DEV
               , retries = 100
               , uart = undefined
               , buffer = <<>>
               , sequence_num = 1
               , continuations = dict:new()
               , async_process = undefined
               }).

send(Pid, Packet) ->
    gen_server:call(Pid, {send, Packet}).

send_async(Pid, Packet) ->
    gen_server:cast(Pid, {send_async, Packet}).

start(Options) ->
    gen_start(start, Options).

start_link(Options) ->
    gen_start(start_link, Options).

gen_start(F, Options) ->
    State = parse_options(Options, #state{}),
    #state{name=Name, server_opts=ServerOpts} = State,
    case Name =:= undefined of
        true ->
            gen_server:F(?MODULE, State, ServerOpts);
        false ->
            gen_server:F(Name, ?MODULE, State, ServerOpts)
    end.

parse_options(Options, State) ->
    Fs = lists:zip(record_info(fields, state),
                   lists:seq(2, record_info(size, state))),
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

stop(Name) when is_pid(Name) orelse is_atom(Name) ->
    gen_server:cast(Name, stop).

init(State=#state{dev=Dev, retries=Retries}) ->
    {ok, Uart} = open(Dev, Retries),
    {ok, State#state{uart=Uart}}.

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

name(#state{name=undefined}) ->
    pid_to_list(self());
name(#state{name=Name}) ->
    Name.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

receive_packets(Data, State) ->
    case spherl_cmd:decode_packet(Data) of
        {more, _N} ->
            State#state{buffer=Data};
        {ok, Packet, Rest} ->
            io:format("received ~p~n", [Packet]),
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

open(Dev, Retries) ->
    try uart:open(Dev, [{baud, 115200},
                        {mode, binary},
                        {packet, raw},
                        {active, true},
                        {exit_on_close, true}])
    catch
        error:Err ->
            case Retries > 0 of
                true ->
                    time:sleep(2 * Retries),
                    open(Dev, Retries - 1);
                false ->
                    {error, Err}
            end
    end.
