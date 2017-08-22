:- set_prolog_flag(optimise, true).
:- set_prolog_stack(global, limit(150*10**9)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(semweb/hdt_server)).
