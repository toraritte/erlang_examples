-module(ppool_worker_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(MFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
    % The strategy is simple_one_for_one because children are going to be added
    % automatically and we want to restrict their type.
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 5,
                 period    => 3600},
    ChildSpec =   #{id     => ppool_worker,
                    % Because we use an {M,F,A} tuple to start the worker,
                    % we can use any kind of OTP behaviour here.
                    % ??? Why is this obvious again???
                    start    => {M,F,A},
                    % Two reasons for using temporary restart strategy:
                    % (1) We cannot assume that the workers need to be restarted
                    %     upon failure.
                    % (2) The worker's creator might need access to the worker's PID
                    %     and by restarting the worker the PID will change. The
                    %     complexity will grow pretty fast as we need to track the
                    %     the creator, send notification to it etc.
                    %     I assume it can be done with references but it is another
                    %     story.
                    restart  => temporary,
                    shutdown => 5000,
                    type     => worker,
                    module   => [M]},
    {ok, {SupFlags, [ChildSpec]}}.
