-module(ppool_serv).
-behaviour(gen_server).

-export([start/4, start_link/4, run/2, sync_queue/2,
         async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%% child_spec of the ppool_worker_sup supervisor, as we are starting it from here
-define(SPEC(MFA),
        #{id       => worker_sup,
          start    => {ppool_worker_sup, start_link, [MFA]},
          restart  => temporary,
          shutdown => 10000,
          type     => supervisor,
          module   => [ppool_worker_sup]}).

%%% PUBLIC API
%%% ==========
start(Name, Limit, Sup, MFA) when is_atom(Name),
                                  is_integer(Limit) ->
    gen_server:start({local,Name}, ?MODULE, {Limit, MFA, Sup}, []).

% Sup refers to ppool_sup because we passed its PID in init/1
start_link(Name, Limit, Sup, MFA) when is_atom(Name),
                                       is_integer(Limit) ->
    gen_server:start_link({local,Name}, ?MODULE, {Limit, MFA, Sup}, []).

%% Run a task in the pool and send notification if it can't be started
%% because the pool is full.
run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

%% Run a task in the pool or if it's full, keep the calling process
%% waiting while the task is in the queue, until it can be run.
sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

%% Run a task asynchronously in the pool, as soon as possible. If no
%% place is available, queue it up and run it whenever.
async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

%%% PRIVATE
%%% =======
-record(state, {limit=0,
                sup,              % store the PID of ppool_worker_sup, that we start from this mod
                refs,             % we're tracking workers with monitors and store refs here
                q=queue:new()}).  % jobs that are waiting for an empty slot

%% INIT/1
%% The straight-forward solution below will not work because it causes
%% a deadlock. The way things work with gen_* behaviours is that the
%% process that spawns the behaviour waits until the init/1 function
%% returns before resuming its processing. This means that if we call
%% supervisor:start_child/2 in ppool_serv:init/1 then:
%
%% init({Limit, MFA, Sup}) ->
%%     {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
%%     link(Pid),
%%     {ok, SomeState}.
%
%% ppool_sup
%%      |
%%      | ---------- spawn -----------> ppool_serv
%%      |                                    |
%% (waiting for                            init/1
%%  {ok, SomeState})                         |
%%      |                                    |
%%      | <------- start_child ------------- |
%%      |                                    |
%%      |                             (waiting for start_child to
%%      |                              return with {ok, Pid})
%%      |                                    |
%%      :                                    :

init({Limit,MFA,Sup}) ->
    self() ! {start_worker_supervisor, Sup, MFA}, % ------------------*
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}. %                |
%                                                                     |
%% REMINDER: handle_info/2 takes care of                              |
%%   * timeout messages                                               |
%%   * system messages                                                |
%%   * any other message then a sync/async request (call/cast)        |
handle_info({start_worker_supervisor, Sup, MFA}, S=#state{}) -> % <-*
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup=Pid}};
%% DEQUEUEING
%% Because we are monitoring all spawned processes if we get a DOWN message,
%% then it means that a slot just opened up.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{refs=R}) ->
    io:format("Received down message~n"),
    case gb_sets:is_element(Ref, R) of
        true ->
            handle_down_worker(Ref, S);
        false ->
            {noreply, S}
    end;
handle_info(Msg, State) ->
    io:format("Unknown message: ~p~n",[Msg]),
    {noreply, State}.

handle_down_worker(Ref, S=#state{limit=N, refs=R, sup=WorkerSup}) ->
    case queue:out(S#state.q) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(WorkerSup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewR = gb_sets:insert(NewRef, gb_sets:delete(Ref,R)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewR, q=Q}};
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(WorkerSup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewR = gb_sets:insert(NewRef, gb_sets:delete(Ref,R)),
            {noreply, S#state{refs=NewR, q=Q}};
        {empty, _} ->
            {noreply, S#state{limit=N+1, refs=gb_sets:delete(Ref,R)}}
    end.

%% RUN/2
%% Run a task in the pool and send notification if it can't be started
%% because the pool is full.
handle_call({run, Args}, _From, S=#state{limit=N, sup=WorkerSup, refs=R}) when N > 0 ->
    {ok,Pid} = supervisor:start_child(WorkerSup, Args),
    Ref = erlang:monitor(process, Pid),
    %                                             gb_sets:add(E, Set1) -> Set2
    {reply, {ok, Pid}, S#state{limit= N-1, refs = gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};
%% SYNC_QUEUE/2
%% Run a task in the pool or if it's full, keep the calling process
%% waiting while the task is in the queue, until it can be run.
handle_call({sync, Args}, _From, S=#state{limit=N, sup=WorkerSup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(WorkerSup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({sync, Args}, From, S=#state{limit=N, q=Q}) when N =< 0 ->
    % pushing address and proc args in queue, and leaving sync_queue/2 hanging
    % until we can find a slot to run its process
    {noreply, S#state{q = queue:in({From, Args}, Q)}};
%% STOP/1
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% ASYNC_QUEUE/2
%% Run a task asynchronously in the pool, as soon as possible. If no
%% place is available, queue it up and run it whenever.
handle_cast({async, Args}, S=#state{limit=N, sup=WorkerSup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(WorkerSup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args}, S=#state{limit=N, q=Q}) when N =< 0 ->
    {noreply, S#state{q = queue:in(Args, Q)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.
