%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sample Module
-module(logi_tty_sample).

-compile({parse_transform, logi_transform}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start/0,
         stop/0,
         log/2
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec start() -> ok.
start() ->
    logi_tty:install(debug).

-spec stop() -> ok.
stop() ->
    logi_tty:uninstall().

-spec log(io:format(), [term()]) -> ok.
log(Format, Args) ->
    _ = logi:info(Format, Args),
    ok.
