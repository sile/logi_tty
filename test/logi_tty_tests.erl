%% @copyright 2018 Takeru Ohta <phjgt308@gmail.com>
-module(logi_tty_tests).

-include_lib("eunit/include/eunit.hrl").


%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
headers_test_() ->
    {foreach,
      fun () ->
              {ok, Apps} = application:ensure_all_started(logi_tty),
              Apps
      end,
      fun (Apps) ->
              lists:foreach(fun application:stop/1, Apps)
      end,
     [
      {"基本動作確認",
       fun () ->
               logi_tty:install(info),
               logi:set_headers([{"foo", bar}, {baz, <<"qux">>}]),
               logi:info("hello")
       end}
     ]}.
