%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TTY backend for logi
-module(logi_tty).

-behaviour(logi_backend).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         install/1, install/2,
         uninstall/0, uninstall/1
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([format/5, write/2]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec install(logi:condition_spec()) -> ok | {error, {already_exists, logi:backend()}}.
install(ConditionSpec) ->
    install(ConditionSpec, logi_tty_sup).

-spec install(logi:condition_spec(), logi:backend_ref()) -> ok | {error, {already_exists, logi:backend()}}.
install(ConditionSpec, BackendRef) ->
    logi:add_backend(ConditionSpec, {BackendRef, ?MODULE, []}).

-spec uninstall() -> ok | {error, not_found}.
uninstall() ->
    uninstall(logi_tty_sup).

-spec uninstall(logi:backend_ref()) -> ok | {error, not_found}.
uninstall(BackendRef) ->
    logi:delete_backend(BackendRef).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi:backend(), logi:msg_info(), logi:location(), io:format(), [term()]) -> iodata().
format(_Backend, MsgInfo, Location, Format, Args) ->
    io_lib:format("~s [~s] ~s:~p ~s\n",
                  [format_timestamp(logi_msg_info:get_timestamp(MsgInfo)),
                   logi_msg_info:get_severity(MsgInfo),
                   logi_location:get_module(Location),
                   logi_location:get_line(Location),
                   io_lib:format(Format, Args)]).

%% @private
-spec write(logi:backend(), iodata()) -> ok.
write(_Backend, Msg) ->
    _ = io:format(Msg),
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Year, Month, Day, Hour, Minute, Second]).
