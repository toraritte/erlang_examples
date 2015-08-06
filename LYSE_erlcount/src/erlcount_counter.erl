-module(erlcount_counter).
-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dispatcher, ref, file, re}).

%%% The two interesting sections here are the init/1 callback, where we order
%%% ourselves to start, and then a single handle_info/2 clause where we open
%%% the file (file:read_file(Name)), get a binary back, which we pass to our
%%% new regex_count/2 function, and then send it back with complete/4. We
%%% then stop the worker. The rest is just standard OTP callback stuff.
start_link(DispatcherPid, Ref, FileName, Regex) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Ref, FileName, Regex], []).
 
init([DispatcherPid, Ref, FileName, Regex]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
                ref = Ref,
                file = FileName,
                re = Regex}}.
 
handle_call(_Msg, _From, State) ->
    {noreply, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(start, S = #state{re=Re, ref=Ref}) ->
    {ok, Bin} = file:read_file(S#state.file),
    Count = erlcount_lib:regex_count(Re, Bin),
    erlcount_dispatch:complete(S#state.dispatcher, Re, Ref, Count),
    {stop, normal, S}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
