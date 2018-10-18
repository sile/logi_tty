logi_tty
========

[![Build Status](https://travis-ci.org/sile/logi_tty.svg?branch=master)](https://travis-ci.org/sile/logi_tty)

TTY出力用の[logi](https://github.com/sile/logi)バックエンド

使い方
------

```erlang
%% infoレベル以上のログを出力する
> logi_tty:install(info).

%% デフォルト(logi:default_logger())とは別にアクセスログをnoticeレベルを閾値値に出力する
> logi:start_logger(access_log).
> logi_tty:install(notice).

%% 登録を解除する
> logi_tty:uninstall().
```
