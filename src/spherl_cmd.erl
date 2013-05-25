-module(spherl_cmd).
-export([encode_packet/4, decode_packet/1, interpret_response/2]).
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
-export([set_heading/1,
         set_stabilization/1,
         set_rotation_rate/1,
         set_application_configuration_block/1,
         get_application_configuration_block/0,
         get_chassis_id/0,
         self_level/4,
         set_data_streaming/4,
         configure_collision_detection/4,
         configure_locator/3,
         set_accelerometer_range/1,
         read_locator/0,
         set_rgb_led_output/2,
         set_back_led_output/1,
         get_rgb_led/0,
         roll/3,
         set_raw_motor_values/2,
         set_motion_timeout/1,
         set_option_flags/1,
         get_option_flags/0,
         get_configuration_block/0,
         set_device_mode/1,
         set_configuration_block/1,
         get_device_mode/0,
         run_macro/1,
         save_temporary_macro/1,
         save_macro/1,
         reinit_macro_executive/0,
         abort_macro/0,
         get_macro_status/0,
         set_macro_parameter/1,
         append_macro_chunk/1,
         erase_orbbasic_storage/1,
         append_orbbasic_fragment/2,
         execute_orbbasic_program/2,
         abort_orbbasic_program/0,
         submit_value_to_input_statement/1]).

-record(req, { command :: atom()
             , command_id :: byte()
             , device_id :: byte()
             , data :: binary() }).
-type packet() :: #req{}.
-type word()   :: 0..16#ffff.
-type dword()  :: 0..16#ffffffff.
-type qword()  :: 0..16#ffffffffffffffff.

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
%% async codes
-define(ASYNC_POWER_NOTIFICATIONS, 16#01).
-define(ASYNC_LEVEL_1_DIAGNOSTIC_RESPONSE, 16#02).
-define(ASYNC_SENSOR_DATA_STREAMING, 16#03).
-define(ASYNC_CONFIG_BLOCK_CONTENTS, 16#04).
-define(ASYNC_PRE_SLEEP_WARNING, 16#05).
-define(ASYNC_MACRO_EMIT_MARKER, 16#06).
-define(ASYNC_COLLISION_DETECTED, 16#07).
-define(ASYNC_ORBBASIC_PRINT, 16#08).
-define(ASYNC_ORBBASIC_ERROR_ASCII, 16#09).
-define(ASYNC_ORBBASIC_ERROR_BINARY, 16#0a).
-define(ASYNC_SELF_LEVEL_COMPLETE, 16#0b).

bitflag(Name, Proplist) ->
    bit(proplists:get_value(Name, Proplist, false)).

bit(true) ->
    1;
bit(false) ->
    0.

%% Packets are sent from Client -> Sphero in the following byte format:
%% [ SOP1, SOP2, DID, CID, SEQ, DLEN, <data>, CHK ]
sop2(Answer, ResetTimeout) ->
    2#11111100 bor
        (bit(ResetTimeout) bsl 1) bor
        bit(Answer).

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

encode_packet(#req{command_id=CID,
                   device_id=DID,
                   data=Data},
              SEQ,
              Answer,
              ResetTimeout) when ((CID >= 0 andalso CID =< 255) andalso
                                  (DID >= 0 andalso DID =< 255) andalso
                                  (SEQ >= 0 andalso SEQ =< 255) andalso
                                  is_binary(Data)) ->
    frame_packet(?SOP1,
                 sop2(Answer, ResetTimeout),
                 DID,
                 CID,
                 SEQ,
                 Data).

interpret_response(_Packet, {response, ?RSP_CODE_OK, _SEQ, <<>>}) ->
    %% Simple response
    ok;
interpret_response(#req{command=Command},
                   {response, ?RSP_CODE_OK, _SEQ, Data}) ->
    decode_response(Command, Data);
interpret_response(_Packet, {response, MRSP, _SEQ, Data}) ->
    {error, {error_info(MRSP), Data}}.

decode_response(get_versioning,
                <<16#01, MDL, HW,
                 MSAVer, MSARev,
                 BLMaj:4, BLMin:4,
                 BASMaj:4, BASMin:4,
                 OverMaj:4, OverMin:4>>) ->
    %% This is my actual hardware and what the iOS SDK does
    [{record, 1},
     {model, MDL},
     {hardware, HW},
     {main_sphero_application, {MSAVer, MSARev}},
     {bootloader, {BLMaj, BLMin}},
     {orbBasic, {BASMaj, BASMin}},
     {overlayManager, {OverMaj, OverMin}}];
decode_response(get_versioning,
                <<16#02, MDL, HW,
                 MSAVer, MSARev,
                 BLMaj:4, BLMin:4,
                 BASMaj:4, BASMin:4,
                 MACROMaj:4, MACROMin:4,
                 APIMaj, APIMin>>) ->
    %% This is the current documentation
    [{record, 2},
     {model, MDL},
     {hardware, HW},
     {main_sphero_application, {MSAVer, MSARev}},
     {bootloader, {BLMaj, BLMin}},
     {orbBasic, {BASMaj, BASMin}},
     {macro, {MACROMaj, MACROMin}},
     {api, {APIMaj, APIMin}}];
decode_response(get_bluetooth_info, <<Name:16/binary, MAC:16/binary>>) ->
    [{name, hd(binary:split(Name, [<<0>>]))},
     {mac, format_mac(MAC)}];
decode_response(get_auto_reconnect, <<Flag, Time>>) ->
    [{enabled, Flag =/= 0},
     {seconds, Time}];
decode_response(get_power_state,
                <<16#01, PowerState, BattVoltage:16,
                 NumCharges:16, TimeSinceChg:16>>) ->
    [{power_state, power_state(PowerState)},
     {battery_voltage, BattVoltage},
     {charges, NumCharges},
     {seconds_since_charge, TimeSinceChg}];
decode_response(get_voltage_trip_points, <<VLow:16, VCrit:16>>) ->
    [{low_voltage, VLow},
     {critical_voltage, VCrit}];
decode_response(perform_level_2_diagnostics, Data) ->
    level_2_diagnostics(Data);
decode_response(poll_packet_times, <<T1:32, T2:32, T3:32>>) ->
    {T1, T2, T3};
decode_response(get_chassis_id, <<ChassisID:16>>) ->
    ChassisID;
decode_response(read_locator, <<XPos:16/signed, YPos:16/signed,
                               XVel:16/signed, YVel:16/signed,
                               SOG:16>>) ->
    [{position, {XPos, YPos}},
     {velocity, {XVel, YVel}},
     {speed_over_ground, SOG}];
decode_response(get_rgb_led, <<Red, Green, Blue>>) ->
    {Red, Green, Blue};
decode_response(get_device_mode, <<Mode>>) ->
    case Mode of
        16#00 -> normal;
        16#01 -> user_hack
    end;
decode_response(abort_macro, <<ID, CmdNum:16>>) ->
    [{id, ID},
     {command_num, CmdNum}];
decode_response(get_macro_status, <<ID, CmdNum:16>>) ->
    [{id, ID},
     {command_num, CmdNum}].

error_info(?RSP_CODE_EGEN) ->
    {general, <<"General, non-specific error">>};
error_info(?RSP_CODE_ECHKSUM) ->
    {chksum, <<"Received checksum failure">>};
error_info(?RSP_CODE_EFRAG) ->
    {frag, <<"Received command fragment">>};
error_info(?RSP_CODE_EBAD_CMD) ->
    {bad_cmd, <<"Unknown command ID">>};
error_info(?RSP_CODE_EUNSUPP) ->
    {unsupp, <<"Command currently unsupported">>};
error_info(?RSP_CODE_EBAD_MSG) ->
    {bad_msg, <<"Bad message format">>};
error_info(?RSP_CODE_EPARAM) ->
    {param, <<"Parameter value(s) invalid">>};
error_info(?RSP_CODE_EEXEC) ->
    {exec, <<"Failed to execute command">>};
error_info(?RSP_CODE_EBAD_DID) ->
    {bad_did, <<"Unknown Device ID">>};
error_info(?RSP_CODE_POWER_NOGOOD) ->
    {power_nogood, <<"Voltage too low for reflash operation">>};
error_info(?RSP_CODE_PAGE_ILLEGAL) ->
    {page_illegal, <<"Illegal page number provided">>};
error_info(?RSP_CODE_FLASH_FAIL) ->
    {flash_fail, <<"Page did not reprogram correctly">>};
error_info(?RSP_CODE_MA_CORRUPT) ->
    {ma_corrupt, <<"Main application corrupt">>};
error_info(?RSP_CODE_MSG_TIMEOUT) ->
    {msg_timeout, <<"Msg state machine timed out">>}.

level_2_diagnostics(Data) ->
    %% TODO: These probably aren't worth decoding right now
    Data.

hex(Nibble) ->
    element(1 + (Nibble band 16#0f),
            {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f}).

format_mac(<<X,Rest/binary>>) ->
    [hex(X bsr 4), hex(X) | case Rest of
                                <<>> ->
                                    [];
                                _ ->
                                    [$: | format_mac(Rest)]
                            end].

decode_packet(P = <<?SOP1, ?SOP2_RESPONSE, MRSP, SEQ, DLEN,
                   Payload:DLEN/binary, Rest/binary>>) ->
    DataSize = DLEN - 1,
    <<Data:DataSize/binary, CHK>> = Payload,
    case checksum(<<MRSP, SEQ, DLEN>>, Data) =:= CHK of
        true ->
            {ok, {response, MRSP, SEQ, Data}, Rest};
        false ->
            {error, {invalid_checksum, P}}
    end;
decode_packet(P = <<?SOP1, ?SOP2_ASYNC, IDCODE, DLEN:16,
                   Payload:DLEN/binary, Rest/binary>>) ->
    DataSize = DLEN - 1,
    <<Data:DataSize/binary, CHK>> = Payload,
    case checksum(<<IDCODE, DLEN:16>>, Data) =:= CHK of
        true ->
            {ok, {async, decode_async_packet(IDCODE, Data)}, Rest};
        false ->
            {error, {invalid_checksum, P}}
    end;
decode_packet(<<?SOP1, ?SOP2_RESPONSE, _MRSP, _SEQ, DLEN, _Rest/binary>>) ->
    {more, 5 + DLEN};
decode_packet(<<?SOP1, ?SOP2_ASYNC, _IDCODE, DLEN:16, _Rest/binary>>) ->
    {more, 5 + DLEN};
decode_packet(Rest) when byte_size(Rest) < 5 ->
    {more, undefined};
decode_packet(Rest) ->
    {error, {invalid_packet_header, Rest}}.

power_state(16#01) -> charging;
power_state(16#02) -> ok;
power_state(16#03) -> low;
power_state(16#04) -> critical.

self_level_result(16#00) -> unknown;
self_level_result(16#01) -> timed_out;
self_level_result(16#02) -> sensors_error;
self_level_result(16#03) -> self_level_disabled;
self_level_result(16#04) -> aborted;
self_level_result(16#05) -> charger_not_found;
self_level_result(16#06) -> success.

collision_axis(16#00) -> x;
collision_axis(16#01) -> y.

orbbasic_error(16#01) -> 'Syntax error';
orbbasic_error(16#02) -> 'Unknown statement';
orbbasic_error(16#03) -> 'GOSUB depth exceeded';
orbbasic_error(16#04) -> 'RETURN without GOSUB';
orbbasic_error(16#05) -> 'NEXT without FOR';
orbbasic_error(16#06) -> 'FOR depth exceeded';
orbbasic_error(16#07) -> 'Bad STEP value';
orbbasic_error(16#08) -> 'Divide by zero';
orbbasic_error(16#09) -> 'Bad line number';
orbbasic_error(16#0a) -> 'Illegal index';
orbbasic_error(16#0b) -> 'Expression too complex';
orbbasic_error(16#0c) -> 'Bad numeric parameter';
orbbasic_error(16#0d) -> 'User abort';
orbbasic_error(16#0e) -> 'Out of data'.

decode_async_packet(?ASYNC_POWER_NOTIFICATIONS, <<State>>) ->
    {power_notification, power_state(State)};
decode_async_packet(?ASYNC_LEVEL_1_DIAGNOSTIC_RESPONSE, Data) ->
    {level_1_diagnostic_response, Data};
decode_async_packet(?ASYNC_SENSOR_DATA_STREAMING, Data) ->
    {sensor_data_streaming, [Param || <<Param:16/signed>> <= Data]};
decode_async_packet(?ASYNC_CONFIG_BLOCK_CONTENTS, Data) ->
    {config_block_contents, Data};
decode_async_packet(?ASYNC_PRE_SLEEP_WARNING, Data) ->
    {pre_sleep_warning, Data};
decode_async_packet(?ASYNC_MACRO_EMIT_MARKER,
                    <<Marker, MacroID, CommandNum:16>>) ->
    {macro_emit_marker, {Marker, MacroID, CommandNum}};
decode_async_packet(?ASYNC_COLLISION_DETECTED,
                    <<X:16/signed, Y:16/signed, Z:16/signed,
                     Axis,
                     XMag:16/signed, YMag:16/signed,
                     Speed, Timestamp:32>>) ->
    {collision_detected,
     {{X, Y, Z}, collision_axis(Axis), {XMag, YMag}, Speed, Timestamp}};
decode_async_packet(?ASYNC_ORBBASIC_PRINT, Data) ->
    {orbbasic_print, Data};
decode_async_packet(?ASYNC_ORBBASIC_ERROR_ASCII, Data) ->
    {orbbasic_error_ascii, Data};
decode_async_packet(?ASYNC_ORBBASIC_ERROR_BINARY, <<Line:16, Error:16>>) ->
    {orbbasic_error_binary, {Line, orbbasic_error(Error)}};
decode_async_packet(?ASYNC_SELF_LEVEL_COMPLETE, <<Result>>) ->
    {self_level_complete, self_level_result(Result)}.

%% Core packets
-spec core(atom(), byte()) -> packet().
core(Name, CID) ->
    core(Name, CID, <<>>).

-spec core(atom(), byte(), binary()) -> packet().
core(Command, CID, Data) ->
    #req{command=Command, device_id=?DID_CORE, command_id=CID, data=Data}.

-spec ping() -> packet().
ping() ->
    core(ping, ?CMD_PING).

-spec get_versioning() -> packet().
get_versioning() ->
    core(get_versioning, ?CMD_VERSION).

-spec set_device_name(binary()) -> packet().
set_device_name(Name) ->
    core(set_device_name, ?CMD_SET_BT_NAME, Name).

-spec get_bluetooth_info() -> packet().
get_bluetooth_info() ->
    core(get_bluetooth_info, ?CMD_GET_BT_NAME).

-spec set_auto_reconnect(boolean(), byte()) -> packet().
set_auto_reconnect(Enabled, Seconds)
  when Seconds >= 0 andalso Seconds =< 16#ff ->
    core(set_auto_reconnect,
         ?CMD_SET_AUTO_RECONNECT,
         <<(bit(Enabled)), Seconds>>).

-spec get_auto_reconnect() -> packet().
get_auto_reconnect() ->
    core(get_auto_reconnect, ?CMD_GET_AUTO_RECONNECT).

-spec get_power_state() -> packet().
get_power_state() ->
    core(get_power_state, ?CMD_GET_PWR_STATE).

-spec set_power_notification(boolean()) -> packet().
set_power_notification(Enabled) ->
    core(set_power_notification, ?CMD_SET_PWR_NOTIFY, <<(bit(Enabled))>>).

-spec sleep(word()) -> packet().
sleep(Wakeup) ->
    sleep(Wakeup, 0, 0).

-spec sleep(word(), byte(), word()) -> packet().
sleep(Wakeup, Macro, OrbBasic) ->
    core(sleep, ?CMD_SLEEP, <<Wakeup:16, Macro, OrbBasic:16>>).

-spec get_voltage_trip_points() -> packet().
get_voltage_trip_points() ->
    core(get_voltage_trip_points, ?GET_POWER_TRIPS).

-spec set_voltage_trip_points(675..725, 625..675) -> packet().
set_voltage_trip_points(Vlow, Vcrit)
  when ((Vlow >= 675 andalso Vlow =< 725) andalso
        (Vcrit >= 625 andalso Vcrit =< 675) andalso
        (Vlow - Vcrit >= 25)) ->
    core(set_voltage_trip_points, ?SET_POWER_TRIPS, <<Vlow:16, Vcrit:16>>).

-spec set_inactivity_timeout(60..65535) -> packet().
set_inactivity_timeout(Time) when Time band 16#ffff >= 60 ->
    core(set_inactivity_timeout, ?SET_INACTIVE_TIMER, <<Time:16>>).

jump_to_bootloader() ->
    core(jump_to_bootloader, ?CMD_GOTO_BL).

perform_level_1_diagnostics() ->
    core(perform_level_1_diagnostics, ?CMD_RUN_L1_DIAGS).

perform_level_2_diagnostics() ->
    core(perform_level_2_diagnostics, ?CMD_RUN_L2_DIAGS).

clear_counters() ->
    core(clear_counters, ?CMD_CLEAR_COUNTERS).

-spec assign_time_value(dword()) -> packet().
assign_time_value(Counter) ->
    core(assign_time_value, ?CMD_ASSIGN_TIME, <<Counter:32>>).

-spec poll_packet_times(dword()) -> packet().
poll_packet_times(TxTime) ->
    core(poll_packet_times, ?CMD_POLL_TIMES, <<TxTime:32>>).

%% Sphero packets

sphero(Command, CID) ->
    sphero(Command, CID, <<>>).

sphero(Command, CID, Data) ->
    #req{command=Command, device_id=?DID_SPHERO, command_id=CID, data=Data}.

-spec set_heading(word()) -> packet().
set_heading(Heading) when Heading >= 0 andalso Heading < 360 ->
    sphero(set_heading, ?CMD_SET_CAL, <<Heading:16>>).

-spec set_stabilization(boolean()) -> packet().
set_stabilization(Enabled) ->
    sphero(set_stabilization, ?CMD_SET_STABILIZ, <<(bit(Enabled))>>).

-spec set_rotation_rate(1..255) -> packet().
set_rotation_rate(Rate) when Rate >= 1 andalso Rate =< 255 ->
    sphero(set_rotation_rate, ?CMD_SET_ROTATION_RATE, <<Rate>>).

set_application_configuration_block(Block) when byte_size(Block) =:= 32 ->
    sphero(set_application_configuration_block,
           ?CMD_SET_BALL_REG_WEBSITE,
           Block).

get_application_configuration_block() ->
    sphero(get_application_configuration_block,
           ?CMD_GET_BALL_REG_WEBSITE).

get_chassis_id() ->
    sphero(get_chassis_id, ?CMD_GET_CHASSIS_ID).

-type level_option() :: (
                    %% start starts the routine, false aborts the routine if in
                    %% progress
                    start |
                    %% final_angle rotates to heading equal to beginning
                    %% heading, false stops
                    final_angle |
                    %% sleep goes to sleep after leveling, false stays awake
                    sleep |
                    %% control_system leaves control system on, false leaves off
                    control_system).
-spec self_level(0..90, byte(), byte(), [level_option()]) -> term().
self_level(AngleLimit, Timeout, TrueTime, Options)
  when ((AngleLimit >= 0 andalso AngleLimit =< 90) andalso
        (Timeout >= 0 andalso Timeout =< 255) andalso
        (TrueTime >= 0 andalso TrueTime =< 255)) ->
    sphero(self_level,
           ?CMD_SELF_LEVEL,
           <<0:4
            , (bitflag(control_system, Options)):1
            , (bitflag(sleep, Options)):1
            , (bitflag(final_angle, Options)):1
            , (bitflag(start, Options)):1
            , AngleLimit
            , Timeout
            , TrueTime
            >>).

-type data_streaming_raw_option() :: (left_motor_back_emf_filtered |
                                      right_motor_back_emf_filtered |
                                      magnetometer_z_filtered |
                                      magnetometer_y_filtered |
                                      magnetometer_x_filtered |
                                      gyro_z_filtered |
                                      gyro_y_filtered |
                                      gyro_x_filtered |
                                      accelerometer_z_filtered |
                                      accelerometer_y_filtered |
                                      accelerometer_x_filtered |
                                      imu_yaw_angle_filtered |
                                      imu_roll_angle_filtered |
                                      imu_pitch_angle_filtered |
                                      left_motor_back_emf_raw |
                                      right_motor_back_emf_raw |
                                      magnetometer_z_raw |
                                      magnetometer_y_raw |
                                      magnetometer_x_raw |
                                      gyro_z_raw |
                                      gyro_y_raw |
                                      gyro_x_raw |
                                      accelerometer_z_raw |
                                      accelerometer_y_raw |
                                      accelerometer_x_raw |
                                      quaternion0 |
                                      quaternion1 |
                                      quaternion2 |
                                      quaternion3 |
                                      locator_x |
                                      locator_y |
                                      velocity_x |
                                      velocity_y).
-type data_streaming_convenience_option() :: (imu_angles_filtered_all |
                                              accelerometer_filtered_all |
                                              locator_all |
                                              quaternion_all).

-type data_streaming_option() :: (data_streaming_raw_option() |
                                  data_streaming_convenience_option()).
-spec data_streaming_mask([data_streaming_option()]) -> qword().
data_streaming_mask(Options) ->
    lists:foldl(
      fun (Opt, Acc) -> data_streaming_mask_val(Opt) bor Acc end,
      0,
      Options).

-spec data_streaming_mask_val(data_streaming_option()) -> qword().
data_streaming_mask_val(Opt) ->
    case Opt of
        left_motor_back_emf_filtered  -> 16#0000000000000020;
        right_motor_back_emf_filtered -> 16#0000000000000040;
        magnetometer_z_filtered       -> 16#0000000000000080;
        magnetometer_y_filtered       -> 16#0000000000000100;
        magnetometer_x_filtered       -> 16#0000000000000200;
        gyro_z_filtered               -> 16#0000000000000400;
        gyro_y_filtered               -> 16#0000000000000800;
        gyro_x_filtered               -> 16#0000000000001000;
        accelerometer_z_filtered      -> 16#0000000000002000;
        accelerometer_y_filtered      -> 16#0000000000004000;
        accelerometer_x_filtered      -> 16#0000000000008000;
        imu_yaw_angle_filtered        -> 16#0000000000010000;
        imu_roll_angle_filtered       -> 16#0000000000020000;
        imu_pitch_angle_filtered      -> 16#0000000000040000;
        left_motor_back_emf_raw       -> 16#0000000000200000;
        right_motor_back_emf_raw      -> 16#0000000000400000;
        magnetometer_z_raw            -> 16#0000000000800000;
        magnetometer_y_raw            -> 16#0000000001000000;
        magnetometer_x_raw            -> 16#0000000002000000;
        gyro_z_raw                    -> 16#0000000004000000;
        gyro_y_raw                    -> 16#0000000008000000;
        gyro_x_raw                    -> 16#0000000010000000;
        accelerometer_z_raw           -> 16#0000000020000000;
        accelerometer_y_raw           -> 16#0000000040000000;
        accelerometer_x_raw           -> 16#0000000080000000;
        quaternion0                   -> 16#8000000000000000;
        quaternion1                   -> 16#4000000000000000;
        quaternion2                   -> 16#2000000000000000;
        quaternion3                   -> 16#1000000000000000;
        locator_x                     -> 16#0800000000000000;
        locator_y                     -> 16#0400000000000000;
        velocity_x                    -> 16#0100000000000000;
        velocity_y                    -> 16#0080000000000000;
        imu_angles_filtered_all       -> 16#0000000000070000;
        accelerometer_filtered_all    -> 16#000000000000E000;
        locator_all                   -> 16#0D80000000000000;
        quaternion_all                -> 16#F000000000000000;
        _                             -> throw({unknown_option, Opt})
    end.

-spec set_data_streaming(word(), word(), byte(), [data_streaming_option()]) ->
    packet().
set_data_streaming(DivMaxRate, NumFrames, Count, Options) ->
    <<Mask2:32, Mask:32>> = <<(data_streaming_mask(Options)):64>>,
    sphero(set_data_streaming,
           ?CMD_SET_DATA_STREAMING,
           <<DivMaxRate:16, NumFrames:16, Mask:32, Count, Mask2:32>>).

-type collision_detection_method() :: on | off.
-spec configure_collision_detection(collision_detection_method(),
                                    {byte(), byte()},
                                    {byte(), byte()},
                                    byte()) -> packet().
configure_collision_detection(Method, {Xt, Yt}, {Xspd, Yspd}, DeadTime10Msec) ->
    sphero(configure_collision_detection,
           ?CMD_SET_COLLISION_DET,
           << (case Method of on -> 1; off -> 0 end)
            , Xt, Yt
            , Xspd, Yspd
            , DeadTime10Msec >>).

-type locator_option() :: autocorrect_yaw_tare.
-spec locator_option([locator_option()]) -> byte().
locator_option([autocorrect_yaw_tare]) -> 1;
locator_option([]) -> 0.

-spec configure_locator({word(), word()}, word(), [locator_option()]) ->
    packet().
configure_locator({X, Y}, YawTare, Options) ->
    sphero(configure_locator,
           ?CMD_LOCATOR,
           << (locator_option(Options)), X:16, Y:16, YawTare:16 >>).

-spec set_accelerometer_range(0..3) -> packet().
set_accelerometer_range(RangeIdx) when RangeIdx >= 0 andalso RangeIdx =< 3 ->
    sphero(set_accelerometer_range,
           ?CMD_SET_ACCELERO,
           <<RangeIdx>>).

read_locator() ->
    sphero(read_locator, ?CMD_READ_LOCATOR).

-type rgb() :: {byte(), byte(), byte()}.
-spec set_rgb_led_output(rgb(), boolean()) -> packet().
set_rgb_led_output({R, G, B}, SaveAsUserLEDColor) ->
    sphero(set_rgb_led_output,
           ?CMD_SET_RGB_LED,
           <<R, G, B, (bit(SaveAsUserLEDColor))>>).

-spec set_back_led_output(byte()) -> packet().
set_back_led_output(Brightness) ->
    sphero(set_back_led_output,
           ?CMD_SET_BACK_LED,
           <<Brightness>>).

get_rgb_led() ->
    sphero(get_rgb_led, ?CMD_GET_RGB_LED).

-type roll_option() :: (normal |
                        rotate_in_place |
                        force_fast_rotation |
                        commence_optimal_braking).
-spec roll(byte(), word(), roll_option()) -> packet().
roll(Speed, Heading, State) ->
    sphero(roll,
           ?CMD_ROLL,
           << Speed
            , Heading:16
            , (case State of
                   normal when Speed > 0            -> 1;
                   rotate_in_place when Speed =:= 0 -> 1;
                   force_fast_rotation              -> 2;
                   commence_optimal_braking         -> 0
               end)
            >>).

-type motor_mode() :: off | forward | reverse | brake | ignore.
-spec motor_mode(motor_mode()) -> byte().
motor_mode(Mode) ->
    case Mode of
        off     -> 0;
        forward -> 1;
        reverse -> 2;
        brake   -> 3;
        ignore  -> 4
    end.

-spec set_raw_motor_values({motor_mode(), byte()}, {motor_mode(), byte()}) ->
    packet().
set_raw_motor_values({LMode, LVal}, {RMode, RVal}) ->
    sphero(set_raw_motor_values,
           ?CMD_SET_RAW_MOTORS,
           << (motor_mode(LMode)), LVal
            , (motor_mode(RMode)), RVal
            >>).

-spec set_motion_timeout(word()) -> packet().
set_motion_timeout(TimeMSec) ->
    sphero(set_motion_timeout,
           ?CMD_SET_MOTION_TO,
           <<TimeMSec:16>>).

-type option_flag() :: (
                   %% Set to prevent Sphero from immediately going to sleep when
                   %% placed in the charger and connected over Bluetooth.
                   prevent_sleep |
                   %% Set to enable Vector Drive, that is, when Sphero is
                   %% stopped and a new roll command is issued it achieves the
                   %% heading before moving along it.
                   enable_vector_drive |
                   %% Set to disable self-leveling when Sphero is inserted into
                   %% the charger.
                   disable_self_leveling_on_charger |
                   %% Set to force the tail LED always on.
                   force_tail_led_on |
                   %% Set to enable motion timeouts (see set_motion_timeout/1)
                   enable_motion_timeouts |
                   %% Set to enable retail Demo Mode (when placed in the
                   %% charger, ball runs a slow rainbow macro for 60 minutes
                   %% and then goes to sleep).
                   enable_retail_demo_mode).
option_flag_val(Opt) ->
    case Opt of
        prevent_sleep                    -> 16#00000001;
        enable_vector_drive              -> 16#00000002;
        disable_self_leveling_on_charger -> 16#00000004;
        force_tail_led_on                -> 16#00000008;
        enable_motion_timeouts           -> 16#00000010;
        enable_retail_demo_mode          -> 16#00000020
    end.

option_flags(Options) ->
    lists:foldl(fun (Opt, Acc) -> option_flag_val(Opt) bor Acc end, 0, Options).

-spec set_option_flags([option_flag()]) -> packet().
set_option_flags(Options) ->
    sphero(set_option_flags,
           ?CMD_SET_OPTIONS_FLAG,
           <<(option_flags(Options)):32>>).

get_option_flags() ->
    sphero(get_option_flags, ?CMD_GET_OPTIONS_FLAG).

get_configuration_block() ->
    sphero(get_configuration_block, ?CMD_GET_CONFIG_BLK).

-type device_mode() :: normal | user_hack.
-spec set_device_mode(device_mode()) -> packet().
set_device_mode(Mode) ->
    sphero(set_device_mode,
           ?CMD_SET_DEVICE_MODE,
           <<(case Mode of normal -> 0; user_hack -> 1 end)>>).

-spec set_configuration_block(binary()) -> packet().
set_configuration_block(Block) when byte_size(Block) =:= 254 ->
    %% TODO: This may need special treatment for DLEN
    sphero(set_configuration_block, ?CMD_SET_CFG_BLOCK, Block).

get_device_mode() ->
    sphero(get_device_mode, ?CMD_GET_DEVICE_MODE).

-spec run_macro(byte()) -> packet().
run_macro(Id) ->
    sphero(run_macro, ?CMD_RUN_MACRO, <<Id>>).

save_temporary_macro(Data) when byte_size(Data) =< 254 ->
    sphero(save_temporary_macro, ?CMD_SAVE_TEMP_MACRO, Data).

save_macro(Data) when byte_size(Data) =< 254 ->
    sphero(save_macro, ?CMD_SAVE_MACRO, Data).

reinit_macro_executive() ->
    sphero(reinit_macro_executive, ?CMD_INIT_MACRO_EXECUTIVE).

abort_macro() ->
    sphero(abort_macro, ?CMD_ABORT_MACRO).

get_macro_status() ->
    sphero(get_macro_status, ?CMD_MACRO_STATUS).

-type macro_parameter() :: ({delay1, word()} |
                            {delay2, word()} |
                            {speed1, byte()} |
                            {speed2, byte()} |
                            {loops, byte()}).
-spec set_macro_parameter(macro_parameter()) -> packet().
set_macro_parameter(Param) ->
    sphero(set_macro_parameter,
           ?CMD_SET_MACRO_PARAM,
           (case Param of
                {delay1, D} -> <<0, D:16>>;
                {delay2, D} -> <<1, D:16>>;
                {speed1, S} -> <<2, S, 0>>;
                {speed2, S} -> <<3, S, 0>>;
                {loops, L}  -> <<4, L, 0>> end)).

append_macro_chunk(Data) when byte_size(Data) =< 254 ->
    sphero(append_macro_chunk, ?CMD_APPEND_TEMP_MACRO_CHUNK, Data).

-spec erase_orbbasic_storage(byte()) -> packet().
erase_orbbasic_storage(Val) ->
    sphero(erase_orbbasic_storage, ?CMD_ERASE_ORBBAS, <<Val>>).

-spec append_orbbasic_fragment(byte(), binary()) -> packet().
append_orbbasic_fragment(Area, Code) when byte_size(Code) =< 253 ->
    sphero(append_orbbasic_fragment, ?CMD_APPEND_FRAG, <<Area, Code/binary>>).

-spec execute_orbbasic_program(byte(), word()) -> packet().
execute_orbbasic_program(Area, StartLine) ->
    sphero(execute_orbbasic_program, ?CMD_EXEC_ORBBAS, <<Area, StartLine:16>>).

abort_orbbasic_program() ->
    sphero(abort_orbbasic_program, ?CMD_ABORT_ORBBAS).

-spec submit_value_to_input_statement(dword()) -> packet().
submit_value_to_input_statement(Val) ->
    sphero(submit_value_to_input_statement, ?CMD_ANSWER_INPUT, <<Val:32>>).
