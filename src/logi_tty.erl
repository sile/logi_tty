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
-export([write/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv install(logi:default_logger(), ConditionSpec)
-spec install(logi_condition:spec()) -> ok.
install(ConditionSpec) ->
    install(logi:default_logger(), ConditionSpec).

%% @doc TTY出力用のログバックエンドをLoggerに登録する
%%
%% 既に登録の場合は、内容が更新される
-spec install(logi:logger(), logi_condition:spec()) -> ok.
install(Logger, ConditionSpec) ->
    logi:set_backend(Logger, {logi_tty_sup, ?MODULE, []}, ConditionSpec).

%% @equiv uninstall(logi:default_logger())
-spec uninstall() -> ok.
uninstall() ->
    uninstall(logi:default_logger()).

%% @doc バックエンドの登録を解除する
%%
%% バックエンドが未登録の場合は、エラーとはならずに単に無視される
-spec uninstall(logi:logger()) -> ok.
uninstall(Logger) ->
    logi:delete_backend(Logger, logi_tty_sup).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_backend:backend(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> any().
write(_Backend, Location, MsgInfo, Format, Args) ->
    %% TODO: formatterを指定可能にする
    io:format("~s [~s] ~p ~p ~s:~p [~s] ~s" ++ format_omitted(logi_msg_info:get_omitted_count(MsgInfo)) ++ "\n",
              [format_timestamp(logi_msg_info:get_timestamp(MsgInfo)),
               logi_msg_info:get_severity(MsgInfo),
               logi_location:get_node(Location),
               logi_location:get_process(Location),
               logi_location:get_module(Location),
               logi_location:get_line(Location),
               format_headers(logi_msg_info:get_headers(MsgInfo)),
               [io_lib:format(Format, Args)]]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).

-spec format_headers(logi:headers()) -> iodata().
format_headers(Headers) ->
    string:join([[atom_to_list(K),"=",to_string(V)] || {K, V} <- Headers], ",").

-spec format_omitted(non_neg_integer()) -> string().
format_omitted(0) -> "";
format_omitted(N) -> " (" ++ integer_to_list(N) ++ " omitted)".

-spec to_string(term()) -> string().
to_string(V) when is_binary(V)   -> binary_to_list(V);
to_string(V) when is_atom(V)     -> atom_to_list(V);
to_string(V) when is_integer(V)  -> integer_to_list(V);
to_string(V) when is_float(V)    -> float_to_list(V);
to_string(V) when is_function(V) -> erlang:fun_to_list(V);
to_string(V) when is_pid(V)      -> erlang:pid_to_list(V);
to_string(V) when is_port(V)     -> erlang:port_to_list(V);
to_string(V) when is_reference(V)-> erlang:ref_to_list(V);
to_string(V) when is_list(V)     ->
    IsNonNegInteger = fun (C) -> is_integer(C) andalso C >= 0 end,
    case lists:all(IsNonNegInteger, V) of
        true  -> V;
        false -> lists:flatten(io_lib:format("~w", [V]))
    end;
to_string(V) ->
    lists:flatten(io_lib:format("~w", [V])).
