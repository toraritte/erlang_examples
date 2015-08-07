-module(erlcount_dispatch).
-behaviour(gen_fsm).

-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(POOL, erlcount).
-record(data, {regex=[], refs=[]}).

%%% PUBLIC API
%%% ==========
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%     *---------------*            *---------------*
%     |  dispatching  |<--*    *-->|  listening    |
%     *---------------*   |    |   *---------------*
%    /   ^       |        |    |
%    |   |       |       <1>  <1>
%    V   |       |        |    |
%   get_files    |        \    /
%                |         \  /
%           dispatch        \/
%                \           |
%                 \          |
%                  *---> results from
%                        ppool workers

% Match it on the diagram: <1>
complete(Pid, Regex, Ref, Count) ->
    % The results will be handled in whatever state this FSM is in
    % using handle_event/3
    gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

init([]) ->
    {ok, Regexes} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(max_files),
    % ?POOL aka "erlcount" refers to the name of the process pool we're
    % about to set up.
    % NOT erlcount.erl in this directory!
    ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
    % Checking that regexes are valid. If we don't do it here then we
    % have to do it in the workers which is much more tedious. At least
    % we can catch it at the beginning.
    case lists:all(fun valid_regex/1, Regexes) of
        true ->
            % Remember, because Erlang's behaviours are pretty much all
            % based on messages, we have to do the ugly step of sending
            % ourselves messages if we want to trigger a function call
            % and do things our way.
            self() ! {start, Dir},
            io:format("~p~n", [Regexes]),
            {ok, dispatching, #data{regex = [{R,0} || R <- Regexes]}};
        false ->
            {stop, invalid_regex}
    end.

%% We sent a message to ourselves in init/1 to get the ball rolling
handle_info({start, Dir}, StateName, StateData) ->
    % Sends an event asynchronously to the gen_fsm FsmRef and returns ok
    % immediately. The gen_fsm will call Module:StateName/2 to handle
    % the event, where StateName is the name of the current state of
    % the gen_fsm.
    % init/1 has already run, so we're in the clear and in 'dispatching'
    % state.
    %                  FsmRef, Event
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
    %                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
    % Because we wrote our recursive directory-traversing function,
    % erlcount_lib:find_erl/2 will run until it returns
    %
    % * 'done' (on the first run it means that there were no .erl
    %           files in the current dir)
    % * {continue, File, Fun_to_continue_find_erl}
    %          (we found the first .erl file, moving on)
    {next_state, StateName, StateData}.

dispatching({continue, File, Continuation}, StateData = #data{regex=Regexes, refs=Refs}) ->
    F = fun({Regex, _Count}, NewRefs) ->
           Ref = make_ref(),
           % Before I forget what's happening here:
           %    Every PPOOL process pool is a SIMPLE_ONE_FOR_ONE supervisor and
           % ppool:start_pool/3 on line 41 sets up a process pool with the name
           % "erlcount" to process MaxFiles amount of files simultaneously, and
           % also initializes the type of processes it will start with the MFA 
           % tuple of {erlcount_counter, start_link, []}.
           %    The command below at one point translates to
           % supervisor:start_child(Sup, Args)
           %    that will call the child in the form of
           % spawn_link(erlcount_counter, start_link, []++[self(), Ref, File, Regex]).
           ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
           [Ref|NewRefs]
        end,
    NewRefs = lists:foldl(F, Refs,Regexes),
    % Again, we have to send messages to ourselves to continue. The variable
    % Continuation jumps back into erlcount_lib:find_erl/2 where we left of
    % and provides ammunition for the next iteration either by returning
    % "done" or "another {continue, File, Continuation}.
    gen_fsm:send_event(self(), Continuation()),
    {next_state, dispatching, StateData#data{refs=NewRefs}};
dispatching(done, StateData) ->
    % Calling listening/2 state EXPLICITLY.
    % see why in main README.md section.
    listening(done, StateData).

listening(done, #data{regex=Regexes, refs=[]}) ->
    [io:format("Regex ~s has ~p results~n", [R,C]) || {R,C} <- Regexes],
    {stop, normal, done};
listening(done, Data) ->
    {next_state, listening, Data}.

handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex=Re, refs=Refs}) ->
    {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
    NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount+Count}),
    NewData = Data#data{regex=NewRe, refs=Refs--[Ref]},
    case State of
        dispatching ->
            {next_state, dispatching, NewData};
        listening ->
            listening(done, NewData)
    end.

handle_sync_event(Event, _From, State, Data) ->
    io:format("Unexpected event: ~p~n", [Event]),
    {next_state, State, Data}.
 
terminate(_Reason, _State, _Data) ->
    ok.
 
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% PRIVATE
%%% =======
valid_regex(Regex) ->
    % running the regex on an empty string because it's faster
    try re:run("",Regex) of
        _ -> true
    catch
        error:badarg -> false
    end.
