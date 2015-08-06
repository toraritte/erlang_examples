-module(ppool_sup).
-behaviour(supervisor).

-export([start_link/3, init/1]).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 1,
                 period    => 3600},
    ChildSpec =   #{id     => serv,
                    start    => {ppool_serv, start_link, [Name, Limit, self(), MFA]},
                    restart  => permanent,
                    shutdown => 5000,
                    type     => worker,
                    module   => [ppool_serv]},
    {ok, {SupFlags, [ChildSpec]}}.
