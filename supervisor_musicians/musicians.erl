-module(musicians).
-behaviour(gen_server).

%% public API
-export([start_link/2, stop/1]).
%% callbacks needed by gen_server
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {name = "", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
    gen_server:start_link({local,Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
    % why not use gen_server:stop/1 ?
    gen_server:call(Role, stop).
    % It would be perfectly reasonable but it only returns the atom 'ok',
    % and this whole module is made to show how supervisor properties work
    % therefore we need a little bit more verbose.
    %
    %   gen_server:stop/{1,3} -> ok
    %
    %   stop(Role)
    %     |
    %     *---> gen_server:call(Role, stop)
    %             |
    %             *---> handle_call(stop, _From, S) -> 
    %                     |
    %                     |      {stop, Reason, Reply, NewState}
    %                     *----->{stop, normal, ok,    S}-------*
    %                                                           |
    %                             terminate(normal, S)  <-------*
    %                               |
    %                               *---> print out state information
    %                                     about the exiting server

init([Role, Skill]) ->
    % http://learnyousomeerlang.com/clients-and-servers
    % "terminate/2 will also be called when its parent (the process
    %  that spawned it) dies, IF AND ONLY IF the gen_server is trapping
    %  exits."
    process_flag(trap_exit, true),
    % sets a seed for random number generation for the life of the process
    rand:seed(exs1024),
    TimeToPlay = rand:uniform(3000),
    % scrapping pick_name
    Name = "MUSICIAN #" ++ integer_to_list(rand:uniform(27)),
    StrRole = atom_to_list(Role),
    io:format("~s - ~s - entered~n", [Name, StrRole]),
    {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

%% The only call we expect is to stop the particular musician server,
%% and if we receive and unexpected message, we do not reply to it and
%% the caller will crash. Not our problem.
handle_call(stop, _From, S=#state{}) ->
    {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
    {noreply, S, ?DELAY}.

%% When the handle_call/3 and handle_cast/2 calls time out,
%% the execution continues in handle_info/2 callback.
%%
%% Musician server instances with 'good' skills never miss
%% a beat, while the 'bad' ones have 20 percent chance to
%% hit a bad note, causing them to crash.
handle_info(timeout, S=#state{name=Name, role=R, skill=good}) ->
    io:format("~s (~s) produced sound~n", [Name, R]),
    {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=Name,role=R, skill=bad}) ->
    case rand:uniform(5) of
        1 ->
            io:format("~s (~s) played a false note. Goodbye!~n", [Name, R]),
            {stop, bad_note, S};
        _ ->
            io:format("~s (~s) produced sound~n", [Name, R]),
            {noreply, S, ?DELAY}
    end;
handle_info(_Message, S) ->
    {noreply, S, ?DELAY}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(normal, #state{name=N, role=R}) ->
    io:format("~s (~s) left.~n",[N, R]);
terminate(bad_note, S) ->
    io:format("Viszlat ~s (~s)!~n",[S#state.name, S#state.role]);
terminate(shutdown, S) ->
    io:format("The manager is mad and fired everyone. ~s (~s) visszement a balettbe ugralni~n", [S#state.name, S#state.role]);
terminate(Reason, S) ->
    io:format("~s (~s) has been kicked out because of ~p~n", [S#state.name, S#state.role, Reason]).
