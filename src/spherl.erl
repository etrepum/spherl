-module(spherl).
-export([open/0, open/1, open/2,
         encode_packet/1, decode_packet/1, send_packet/3, recv_packet/1]).
%% Based on Sphero API 1.40
%% https://github.com/orbotix/DeveloperResources/blob/master/docs/
%% Core packets
-export([ping/0,
         get_versioning/0,
         set_device_name/1,
         get_bluetooth_info/0,
         set_auto_reconnect/2,
         get_auto_reconnect/0,
         get_power_state/0,
         set_power_notification/1,
         sleep/1,
         sleep/3,
         get_voltage_trip_points/0,
         set_voltage_trip_points/2,
         set_inactivity_timeout/1,
         jump_to_bootloader/0,
         perform_level_1_diagnostics/0,
         perform_level_2_diagnostics/0,
         clear_counters/0,
         assign_time_value/1,
         poll_packet_times/1
        ]).
%% Sphero packets
-export([set_heading/1]).%%,
         %% set_stabilization/1,
         %% set_rotation_rate/1,
         %% set_application_configuration_block/1,
         %% get_application_configuration_block/0,
         %% get_chassis_id/0,
         %% self_level/4,
         %% set_data_streaming/4,
         %% configure_collision_detection/4,
         %% configure_locator/3,
         %% set_accelerometer_range/1,
         %% read_locator/0,
         %% set_rgb_led_output/2,
         %% set_back_led_output/1,
         %% get_rgb_led/0,
         %% roll/4,
         %% set_raw_motor_values/4,
         %% set_motion_timeout/1,
         %% set_option_flags/1,
         %% get_option_flags/0,
         %% get_configuration_block/0,
         %% set_device_mode/1,
         %% set_configuration_block/1,
         %% get_device_mode/0,
         %% run_macro/1,
         %% save_temporary_macro/1,
         %% save_macro/1,
         %% reinit_macro_executive/0,
         %% abort_macro/0,
         %% get_macro_status/0,
         %% set_macro_parameter/3,
         %% append_macro_chunk/1,
         %% erase_orbbasic_storage/1,
         %% append_orbbasic_fragment/3,
         %% execute_orbbasic_program/3,
         %% abort_orbbasic_program/0,
         %% submit_value_to_input_statement/1]).

-define(DEFAULT_DEV, "/dev/tty.Sphero-PYO-RN-SPP").

-define(SOP1, 16#ff).
-define(SOP2_RESPONSE, 16#ff).
-define(SOP2_ASYNC, 16#fe).

%% Appendix A: Enumerated Codes Quick Reference
-define(DID_CORE, 16#00).
-define(DID_BOOTLOADER, 16#01).
-define(DID_SPHERO, 16#02).
%% core
-define(CMD_PING, 16#01).
-define(CMD_VERSION, 16#02).
-define(CMD_SET_BT_NAME, 16#10).
-define(CMD_GET_BT_NAME, 16#11).
-define(CMD_SET_AUTO_RECONNECT, 16#12).
-define(CMD_GET_AUTO_RECONNECT, 16#13).
-define(CMD_GET_PWR_STATE, 16#20).
-define(CMD_SET_PWR_NOTIFY, 16#21).
-define(CMD_SLEEP, 16#22).
-define(GET_POWER_TRIPS, 16#23).
-define(SET_POWER_TRIPS, 16#24).
-define(SET_INACTIVE_TIMER, 16#25).
-define(CMD_GOTO_BL, 16#30).
-define(CMD_RUN_L1_DIAGS, 16#40).
-define(CMD_RUN_L2_DIAGS, 16#41).
-define(CMD_CLEAR_COUNTERS, 16#42).
-define(CMD_ASSIGN_TIME, 16#50).
-define(CMD_POLL_TIMES, 16#51).
%% bootloader
-define(BEGIN_REFLASH, 16#02).
-define(HERE_IS_PAGE, 16#03).
-define(LEAVE_BOOTLOADER, 16#04).
-define(IS_PAGE_BLANK, 16#05).
-define(CMD_ERASE_USER_CONFIG, 16#06).
%% sphero
-define(CMD_SET_CAL, 16#01).
-define(CMD_SET_STABILIZ, 16#02).
-define(CMD_SET_ROTATION_RATE, 16#03).
-define(CMD_SET_BALL_REG_WEBSITE, 16#04).
-define(CMD_GET_BALL_REG_WEBSITE, 16#05).
-define(CMD_REENABLE_DEMO, 16#06).
-define(CMD_GET_CHASSIS_ID, 16#07).
-define(CMD_SET_CHASSIS_ID, 16#08).
-define(CMD_SELF_LEVEL, 16#09).
-define(CMD_SET_VDL, 16#0a).
-define(CMD_SET_DATA_STREAMING, 16#11).
-define(CMD_SET_COLLISION_DET, 16#12).
-define(CMD_LOCATOR, 16#13).
-define(CMD_SET_ACCELERO, 16#14).
-define(CMD_READ_LOCATOR, 16#15).
-define(CMD_SET_RGB_LED, 16#20).
-define(CMD_SET_BACK_LED, 16#21).
-define(CMD_GET_RGB_LED, 16#22).
-define(CMD_ROLL, 16#30).
-define(CMD_BOOST, 16#31).
-define(CMD_MOVE, 16#32).
-define(CMD_SET_RAW_MOTORS, 16#33).
-define(CMD_SET_MOTION_TO, 16#34).
-define(CMD_SET_OPTIONS_FLAG, 16#35).
-define(CMD_GET_OPTIONS_FLAG, 16#36).
-define(CMD_GET_CONFIG_BLK, 16#40).
-define(CMD_SET_DEVICE_MODE, 16#42).
-define(CMD_SET_CFG_BLOCK, 16#43).
-define(CMD_GET_DEVICE_MODE, 16#44).
-define(CMD_RUN_MACRO, 16#50).
-define(CMD_SAVE_TEMP_MACRO, 16#51).
-define(CMD_SAVE_MACRO, 16#52).
-define(CMD_INIT_MACRO_EXECUTIVE, 16#54).
-define(CMD_ABORT_MACRO, 16#55).
-define(CMD_MACRO_STATUS, 16#56).
-define(CMD_SET_MACRO_PARAM, 16#57).
-define(CMD_APPEND_TEMP_MACRO_CHUNK, 16#58).
-define(CMD_ERASE_ORBBAS, 16#60).
-define(CMD_APPEND_FRAG, 16#61).
-define(CMD_EXEC_ORBBAS, 16#62).
-define(CMD_ABORT_ORBBAS, 16#63).
-define(CMD_ANSWER_INPUT, 16#64).
%% response codes
-define(RSP_CODE_OK, 16#00).
-define(RSP_CODE_EGEN, 16#01).
-define(RSP_CODE_ECHKSUM, 16#02).
-define(RSP_CODE_EFRAG, 16#03).
-define(RSP_CODE_EBAD_CMD, 16#04).
-define(RSP_CODE_EUNSUPP, 16#05).
-define(RSP_CODE_EBAD_MSG, 16#06).
-define(RSP_CODE_EPARAM, 16#07).
-define(RSP_CODE_EEXEC, 16#08).
-define(RSP_CODE_EBAD_DID, 16#09).
-define(RSP_CODE_POWER_NOGOOD, 16#31).
-define(RSP_CODE_PAGE_ILLEGAL, 16#32).
-define(RSP_CODE_FLASH_FAIL, 16#33).
-define(RSP_CODE_MA_CORRUPT, 16#34).
-define(RSP_CODE_MSG_TIMEOUT, 16#35).

send_packet(P, Seq, Packet) ->
    uart:send(P, encode_packet([{sequence_num, Seq}, answer | Packet])).

recv_packet(P) ->
    recv_packet(P, 6, <<>>).

recv_packet(P, N, B) when byte_size(B) < N ->
    {ok, D} = uart:recv(P, N - byte_size(B)),
    recv_packet(P, N, <<B/binary, D/binary>>);
recv_packet(P, N, B) ->
    case decode_packet(B) of
        {more, N1} when N1 > N ->
            recv_packet(P, N1, B);
        Res ->
            Res
    end.

open() ->
    open(?DEFAULT_DEV, 100).

open(Dev) ->
    open(Dev, 100).

open(Dev, Retries) ->
    try uart:open(Dev, [{baud, 115200}, {mode, binary} ])
    catch
        error:Err ->
            case Retries > 0 of
                true ->
                    open(Dev, Retries - 1);
                false ->
                    {error, Err}
            end
    end.

bitflag(Name, Proplist) ->
    bit(proplists:get_value(Name, Proplist, false)).

bit(true) ->
    1;
bit(false) ->
    0.

%% Packets are sent from Client -> Sphero in the following byte format:
%% [ SOP1, SOP2, DID, CID, SEQ, DLEN, <data>, CHK ]
sop2(Opts) ->
    2#11111100 bor
        (bitflag(reset_timeout, Opts) bsl 1) bor
        bitflag(answer, Opts).

device_id(Opts) ->
    proplists:get_value(device_id, Opts, 0).

command_id(Opts) ->
    proplists:get_value(command_id, Opts, 0).

sequence_num(Opts) ->
    proplists:get_value(sequence_num, Opts, 0).

data(Opts) ->
    iolist_to_binary(proplists:get_value(data, Opts, <<>>)).

checksum(Data) ->
    byte_sum(Data, 0) bxor 16#ff.

checksum(Front, Back) ->
    byte_sum(Back, byte_sum(Front, 0)) bxor 16#ff.

byte_sum(<<B, Rest/binary>>, Acc) ->
    byte_sum(Rest, (B + Acc) band 16#ff);
byte_sum(<<>>, Acc) ->
    Acc.

frame_packet(SOP1, SOP2, DID, CID, SEQ, Data) when byte_size(Data) < 16#ff ->
    Payload = <<DID, CID, SEQ, (1 + byte_size(Data)), Data/binary>>,
    <<SOP1, SOP2, Payload/binary, (checksum(Payload))>>.

encode_packet(Opts) ->
    CID = command_id(Opts),
    Data = data(Opts),
    frame_packet(?SOP1,
                 sop2(Opts),
                 device_id(Opts),
                 CID,
                 sequence_num(Opts),
                 Data).

decode_packet(P = <<?SOP1, ?SOP2_RESPONSE, MSRP, SEQ, DLEN,
                   Payload:DLEN/binary, Rest/binary>>) ->
    DataSize = DLEN - 1,
    <<Data:DataSize/binary, CHK>> = Payload,
    case checksum(<<MSRP, SEQ, DLEN>>, Data) =:= CHK of
        true ->
            {ok, {response, MSRP, SEQ, Data}, Rest};
        false ->
            {ok, {error, {invalid_checksum, P}}, Rest}
    end;
decode_packet(P = <<?SOP1, ?SOP2_ASYNC, IDCODE, DLEN:16,
                   Payload:DLEN/binary, Rest/binary>>) ->
    DataSize = DLEN - 1,
    <<Data:DataSize/binary, CHK>> = Payload,
    case checksum(<<IDCODE, DLEN:16>>, Data) =:= CHK of
        true ->
            {ok, {async, IDCODE, Data}, Rest};
        false ->
            {ok, {error, {invalid_checksum, P}}, Rest}
    end;
decode_packet(<<?SOP1, ?SOP2_RESPONSE, _MSRP, _SEQ, DLEN, _Rest/binary>>) ->
    {more, 5 + DLEN};
decode_packet(<<?SOP1, ?SOP2_ASYNC, _IDCODE, DLEN:16, _Rest/binary>>) ->
    {more, 5 + DLEN};
decode_packet(Rest) when byte_size(Rest) < 5 ->
    {more, undefined};
decode_packet(Rest) ->
    {error, {invalid_packet_header, Rest}}.

%% Core packets

core(CID) ->
    [{device_id, ?DID_CORE}, {command_id, CID}].

core(CID, Data) ->
    [{data, Data} | core(CID)].

ping() ->
    core(?CMD_PING).

get_versioning() ->
    core(?CMD_VERSION).

set_device_name(Name) ->
    core(?CMD_SET_BT_NAME, Name).

get_bluetooth_info() ->
    core(?CMD_GET_BT_NAME).

set_auto_reconnect(Enabled, Seconds)
  when Seconds >= 0 andalso Seconds =< 16#ff ->
    core(?CMD_SET_AUTO_RECONNECT, <<(bit(Enabled)), Seconds>>).

get_auto_reconnect() ->
    core(?CMD_GET_AUTO_RECONNECT).

get_power_state() ->
    core(?CMD_GET_PWR_STATE).

set_power_notification(Enabled) ->
    core(?CMD_SET_PWR_NOTIFY, <<(bit(Enabled))>>).

sleep(Wakeup) ->
    sleep(Wakeup, 0, 0).

sleep(Wakeup, Macro, OrbBasic) ->
    core(?CMD_SLEEP, <<Wakeup:16, Macro, OrbBasic:16>>).

get_voltage_trip_points() ->
    core(?GET_POWER_TRIPS).

set_voltage_trip_points(Vlow, Vcrit)
  when ((Vlow >= 675 andalso Vlow =< 725) andalso
        (Vcrit >= 625 andalso Vcrit =< 675) andalso
        (Vlow - Vcrit >= 25)) ->
    core(?SET_POWER_TRIPS, <<Vlow:16, Vcrit:16>>).

set_inactivity_timeout(Time) when Time band 16#ffff >= 60 ->
    core(?SET_INACTIVE_TIMER, <<Time:16>>).

jump_to_bootloader() ->
    core(?CMD_GOTO_BL).

perform_level_1_diagnostics() ->
    core(?CMD_RUN_L1_DIAGS).

perform_level_2_diagnostics() ->
    core(?CMD_RUN_L2_DIAGS).

clear_counters() ->
    core(?CMD_CLEAR_COUNTERS).

assign_time_value(Counter) ->
    core(?CMD_ASSIGN_TIME, <<Counter:32>>).

poll_packet_times(TxTime) ->
    core(?CMD_POLL_TIMES, <<TxTime:32>>).

%% Sphero packets

sphero(CID) ->
    [{device_id, ?DID_SPHERO}, {command_id, CID}].

sphero(CID, Data) ->
    [{data, Data} | sphero(CID)].

set_heading(Heading) ->
    sphero(?CMD_SET_CAL, <<Heading:16>>).
