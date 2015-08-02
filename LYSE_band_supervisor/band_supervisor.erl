-module(band_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
%%% callback for supervisor
-export([init/1]).

start_link(Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

%% In this analogy the supervisor is the band's manager, and we
%% make up three types.
init(lenient) ->
    init(#{strategy  => one_for_one,
      % if more than 3 restarts occur in 60 seconds, the
      % supervisor terminates all its children and then itself
      intensity => 3,
      period    => 60});
init(angry) ->
    init(#{strategy  => rest_for_one,
      % tolerates max 2 restarts in 60 seconds
      intensity => 2,
      period    => 60});
init(jerk) ->
    init(#{strategy  => one_for_all,
      % you got the point
      intensity => 1,
      period    => 60});
init(SupFlags) ->
    {ok, {SupFlags,
           % === ChildSpec_1
           % So the singer is essential, and the band always have to have
          % one, but the bass player is pretty much expendable.
          [#{id       => singer,
             start    => {musicians, start_link, [singer, good]},
             % RESTART STRATEGY REMINDER
             %
             %   [-permanent-] child processes will always be restarted,
             %
             %   [-temporary-] child processes will never be restarted (even when
             %                 the supervisor's restart strategy is rest_for_one
             %                 or one_for_all and a sibling's death causes the
             %                 temporary process to be terminated) and
             %   [-transient-] child processes will be restarted only if they terminate
             %                 abnormally, i.e. with another exit reason than normal,
             %                 shutdown or {shutdown,Term}.
             restart  => permanent,
             shutdown => 1000,
             type     => worker,
             module   => [musicians]},
           % === ChildSpec_2
           #{id       => bass,
             start    => {musicians, start_link, [bass, good]},
             restart  => transient,
             shutdown => 1000,
             type     => worker,
             module   => [musicians]},
           % === ChildSpec_3
           #{id       => drum,
             start    => {musicians, start_link, [drum, bad]},
             restart  => transient,
             shutdown => 1000,
             type     => worker,
             module   => [musicians]},
           % === ChildSpec_4
           #{id       => keytar,
             start    => {musicians, start_link, [keytar, good]},
             restart  => temporary,
             shutdown => 1000,
             type     => worker,
             module   => [musicians]}]}}.

