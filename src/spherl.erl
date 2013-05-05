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
    try uart:open(Dev, [{baud, 115200}, {mode, binary}])
    catch
        error:Err ->
            case Retries > 0 of
                true ->
                    timer:sleep(20),
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
-type packet() :: term().
-type word() :: 0..16#ffff.
-type dword() :: 0..16#ffffffff.
-type qword() :: 0..16#ffffffffffffffff.

core(CID) ->
    [{device_id, ?DID_CORE}, {command_id, CID}].

core(CID, Data) ->
    [{data, Data} | core(CID)].

ping() ->
    core(?CMD_PING).

get_versioning() ->
    core(?CMD_VERSION).

-spec set_device_name(binary()) -> packet().
set_device_name(Name) ->
    core(?CMD_SET_BT_NAME, Name).

get_bluetooth_info() ->
    core(?CMD_GET_BT_NAME).

-spec set_auto_reconnect(boolean(), byte()) -> packet().
set_auto_reconnect(Enabled, Seconds)
  when Seconds >= 0 andalso Seconds =< 16#ff ->
    core(?CMD_SET_AUTO_RECONNECT, <<(bit(Enabled)), Seconds>>).

get_auto_reconnect() ->
    core(?CMD_GET_AUTO_RECONNECT).

get_power_state() ->
    core(?CMD_GET_PWR_STATE).

-spec set_power_notification(boolean()) -> packet().
set_power_notification(Enabled) ->
    core(?CMD_SET_PWR_NOTIFY, <<(bit(Enabled))>>).

-spec sleep(word()) -> packet().
sleep(Wakeup) ->
    sleep(Wakeup, 0, 0).

-spec sleep(word(), byte(), word()) -> packet().
sleep(Wakeup, Macro, OrbBasic) ->
    core(?CMD_SLEEP, <<Wakeup:16, Macro, OrbBasic:16>>).

get_voltage_trip_points() ->
    core(?GET_POWER_TRIPS).

-spec set_voltage_trip_points(675..725, 625..675) -> packet().
set_voltage_trip_points(Vlow, Vcrit)
  when ((Vlow >= 675 andalso Vlow =< 725) andalso
        (Vcrit >= 625 andalso Vcrit =< 675) andalso
        (Vlow - Vcrit >= 25)) ->
    core(?SET_POWER_TRIPS, <<Vlow:16, Vcrit:16>>).

-spec set_inactivity_timeout(60..65535) -> packet().
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

-spec assign_time_value(dword()) -> packet().
assign_time_value(Counter) ->
    core(?CMD_ASSIGN_TIME, <<Counter:32>>).

-spec poll_packet_times(dword()) -> packet().
poll_packet_times(TxTime) ->
    core(?CMD_POLL_TIMES, <<TxTime:32>>).

%% Sphero packets

sphero(CID) ->
    [{device_id, ?DID_SPHERO}, {command_id, CID}].

sphero(CID, Data) ->
    [{data, Data} | sphero(CID)].

-spec set_heading(word()) -> packet().
set_heading(Heading) ->
    sphero(?CMD_SET_CAL, <<Heading:16>>).

-spec set_stabilization(boolean()) -> packet().
set_stabilization(Enabled) ->
    sphero(?CMD_SET_STABILIZ, <<(bit(Enabled))>>).

-spec set_rotation_rate(1..255) -> packet().
set_rotation_rate(Rate) when Rate >= 1 andalso Rate =< 255 ->
    sphero(?CMD_SET_ROTATION_RATE, <<Rate>>).

set_application_configuration_block(Block) when byte_size(Block) =:= 32 ->
    sphero(?CMD_SET_BALL_REG_WEBSITE, Block).

get_application_configuration_block() ->
    sphero(?CMD_GET_BALL_REG_WEBSITE).

get_chassis_id() ->
    sphero(?CMD_GET_CHASSIS_ID).

-type level_option() :: start | final_angle | sleep | control_system.
-spec self_level(0..90, byte(), byte(), [level_option()]) -> term().
self_level(AngleLimit, Timeout, TrueTime, Options)
  when ((AngleLimit >= 0 andalso AngleLimit =< 90) andalso
        (Timeout >= 0 andalso Timeout =< 255) andalso
        (TrueTime >= 0 andalso TrueTime =< 255)) ->
    %% start starts the routine, false aborts the routine if in progress
    %% final_angle rotates to heading equal to beginning heading, false stops
    %% sleep goes to sleep after leveling, false stays awake
    %% control_system leaves control system on, false leaves off
    sphero(?CMD_SELF_LEVEL,
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
    sphero(?CMD_SET_DATA_STREAMING,
           <<DivMaxRate:16, NumFrames:16, Mask:32, Count, Mask2:32>>).

-type collision_detection_method() :: on | off.
-spec configure_collision_detection(collision_detection_method(),
                                    {byte(), byte()},
                                    {byte(), byte()},
                                    byte()) -> packet().
configure_collision_detection(Method, {Xt, Yt}, {Xspd, Yspd}, DeadTime10Msec) ->
    sphero(?CMD_SET_COLLISION_DET,
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
    sphero(?CMD_LOCATOR,
           << (locator_option(Options)), X:16, Y:16, YawTare:16 >>).

-spec set_accelerometer_range(0..3) -> packet().
set_accelerometer_range(RangeIdx) when RangeIdx >= 0 andalso RangeIdx =< 3 ->
    sphero(?CMD_SET_ACCELERO, <<RangeIdx>>).

read_locator() ->
    sphero(?CMD_READ_LOCATOR).

-type rgb() :: {byte(), byte(), byte()}.
-spec set_rgb_led_output(rgb(), boolean()) -> packet().
set_rgb_led_output({R, G, B}, SaveAsUserLEDColor) ->
    sphero(?CMD_SET_RGB_LED, <<R, G, B, (bit(SaveAsUserLEDColor))>>).

-spec set_back_led_output(byte()) -> packet().
set_back_led_output(Brightness) ->
    sphero(?CMD_SET_BACK_LED, <<Brightness>>).

get_rgb_led() ->
    sphero(?CMD_GET_RGB_LED).

-type roll_option() :: (normal |
                        rotate_in_place |
                        force_fast_rotation |
                        commence_optimal_braking).
-spec roll(byte(), word(), roll_option()) -> packet().
roll(Speed, Heading, State) ->
    sphero(?CMD_ROLL,
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
        off -> 0;
        forward -> 1;
        reverse -> 2;
        brake -> 3;
        ignore -> 4
    end.

-spec set_raw_motor_values({motor_mode(), byte()}, {motor_mode(), byte()}) ->
    packet().
set_raw_motor_values({LMode, LVal}, {RMode, RVal}) ->
    sphero(?CMD_SET_RAW_MOTORS,
           << (motor_mode(LMode)), LVal
            , (motor_mode(RMode)), RVal
            >>).

-spec set_motion_timeout(word()) -> packet().
set_motion_timeout(TimeMSec) ->
    sphero(?CMD_SET_MOTION_TO, <<TimeMSec:16>>).

-type option_flag() :: (prevent_sleep |
                        enable_vector_drive |
                        disable_self_leveling_on_charger |
                        force_tail_led_on |
                        enable_motion_timeouts |
                        enable_retail_demo_mode).
option_flag_val(Opt) ->
    case Opt of
        %% Set to prevent Sphero from immediately going to sleep when placed in
        %% the charger and connected over Bluetooth.
        prevent_sleep -> 16#00000001;
        %% Set to enable Vector Drive, that is, when Sphero is stopped and a
        %% new roll command is issued it achieves the heading before moving
        %% along it.
        enable_vector_drive -> 16#00000002;
        %% Set to disable self-leveling when Sphero is inserted into the
        %% charger.
        disable_self_leveling_on_charger -> 16#00000004;
        %% Set to force the tail LED always on.
        force_tail_led_on -> 16#00000008;
        %% Set to enable motion timeouts (see set_motion_timeout/1)
        enable_motion_timeouts -> 16#00000010;
        %% Set to enable retail Demo Mode (when placed in the charger, ball runs
        %% a slow rainbow macro for 60 minutes and then goes to sleep).
        enable_retail_demo_mode -> 16#00000020
    end.

option_flags(Options) ->
    lists:foldl(fun (Opt, Acc) -> option_flag_val(Opt) bor Acc end, 0, Options).

-spec set_option_flags([option_flag()]) -> packet().
set_option_flags(Options) ->
    sphero(?CMD_SET_OPTIONS_FLAG, <<(option_flags(Options)):32>>).

get_option_flags() ->
    sphero(?CMD_GET_OPTIONS_FLAG).

get_configuration_block() ->
    sphero(?CMD_GET_CONFIG_BLK).

-type device_mode() :: normal | user_hack.
-spec set_device_mode(device_mode()) -> packet().
set_device_mode(Mode) ->
    sphero(?CMD_SET_DEVICE_MODE,
           <<(case Mode of normal -> 0; user_hack -> 1 end)>>).

-spec set_configuration_block(binary()) -> packet().
set_configuration_block(Block) when byte_size(Block) =:= 254 ->
    %% TODO: This may need special treatment for DLEN
    sphero(?CMD_SET_CFG_BLOCK, Block).

get_device_mode() ->
    sphero(?CMD_GET_DEVICE_MODE).

-spec run_macro(byte()) -> packet().
run_macro(Id) ->
    sphero(?CMD_RUN_MACRO, <<Id>>).

save_temporary_macro(Data) when byte_size(Data) =< 254 ->
    sphero(?CMD_SAVE_TEMP_MACRO, Data).

save_macro(Data) when byte_size(Data) =< 254 ->
    sphero(?CMD_SAVE_MACRO, Data).

reinit_macro_executive() ->
    sphero(?CMD_INIT_MACRO_EXECUTIVE).

abort_macro() ->
    sphero(?CMD_ABORT_MACRO).

get_macro_status() ->
    sphero(?CMD_MACRO_STATUS).

-type macro_parameter() :: ({delay1, word()} |
                            {delay2, word()} |
                            {speed1, byte()} |
                            {speed2, byte()} |
                            {loops, byte()}).
-spec set_macro_parameter(macro_parameter()) -> packet().
set_macro_parameter(Param) ->
    sphero(?CMD_SET_MACRO_PARAM,
           (case Param of
                {delay1, D} -> <<0, D:16>>;
                {delay2, D} -> <<1, D:16>>;
                {speed1, S} -> <<2, S, 0>>;
                {speed2, S} -> <<3, S, 0>>;
                {loops, L}  -> <<4, L, 0>> end)).

append_macro_chunk(Data) when byte_size(Data) =< 254 ->
    sphero(?CMD_APPEND_TEMP_MACRO_CHUNK, Data).

-spec erase_orbbasic_storage(byte()) -> packet().
erase_orbbasic_storage(Val) ->
    sphero(?CMD_ERASE_ORBBAS, <<Val>>).

-spec append_orbbasic_fragment(byte(), binary()) -> packet().
append_orbbasic_fragment(Area, Code) when byte_size(Code) =< 253 ->
    sphero(?CMD_APPEND_FRAG, <<Area, Code/binary>>).

-spec execute_orbbasic_program(byte(), word()) -> packet().
execute_orbbasic_program(Area, StartLine) ->
    sphero(?CMD_EXEC_ORBBAS, <<Area, StartLine:16>>).

abort_orbbasic_program() ->
    sphero(?CMD_ABORT_ORBBAS).

-spec submit_value_to_input_statement(dword()) -> packet().
submit_value_to_input_statement(Val) ->
    sphero(?CMD_ANSWER_INPUT, <<Val:32>>).
