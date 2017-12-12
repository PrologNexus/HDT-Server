:- set_prolog_flag(optimise, true).%DEV
:- set_prolog_stack(global, limit(150*10**9)).%DEV

:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).

:- use_module(hdt_server).
