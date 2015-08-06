%%% Changed this file to adhere to OTP application standards
%%% To see what has changed:
%%%
%%% (1) vimdiff LYSE_ppool/ppool.erl ./ppool.erl
%%% (2) :diffthis
%%%     Using Tim Pope's unimpaired plugin is easier (cod)
-module(ppool).
-behaviour(application).

-export([start/2, stop/1, start_pool/3,
         run/2, sync_queue/2, async_queue/2, stop_pool/1]).
 
start(normal, _Args) ->
    ppool_supersup:start_link().
 
stop(_State) ->
    ok.
 
start_pool(Name, Limit, {M,F,A}) ->
    ppool_supersup:start_pool(Name, Limit, {M,F,A}).
 
stop_pool(Name) ->
    ppool_supersup:stop_pool(Name).
 
run(Name, Args) ->
    ppool_serv:run(Name, Args).
 
async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
 
sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).
