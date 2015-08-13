-module(uniq_sup).
-behaviour(supervisor).

%%% API
-export([start_link/0]).
%%% supervisor callback
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 % default values
                 intensity => 1,
                 period    => 5},
    UniqServSpec = #{id       => uniq_id,
                     start    => {uniq_serv, start_link, []},
                     restart  => permanent,
                     shutdown => 2000,
                     type     => worker,
                     modules  => [uniq_serv]},
    {ok, {SupFlags, [UniqServSpec]}}.

