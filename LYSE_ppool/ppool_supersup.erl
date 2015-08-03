-module(ppool_supersup).
-behaviour(supervisor).

-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
    % as this is the top level supervisor, we can give it any
    % name without worrying about clashes (at least in this
    % application)
    supervisor:start_link({local,ppool}, ?MODULE, []).

%% The OTP framework provides an elegant shutdown procedure but
%% we cannot use it yet hence the brute force alternative.
stop() ->
    case whereis(ppool) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill);
        _ -> ok
    end.

%% This is going to be a childless supervisor as we are going
%% to add pools to it dynamically.
init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 6,
                 period    => 3600},
    {ok,{SupFlags, []}}.

start_pool(Name, Limit, MFA) ->
    ChildSpec = #{id       => Name,
                  start    => {ppool_sup, start_link, [Name, Limit, MFA]},
                  restart  => permanent,
                  shutdown => 10500,
                  type     => supervisor,
                  modules  => [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).
