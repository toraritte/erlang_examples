http://learnyousomeerlang.com/the-count-of-applications

Purpose of erlcount:
--------------------
Recursively look into some directory, find all Erlang files (ending in
.erl) and then run a regular expression over it to count all instances
of a given string within the modules (we'll try to count how many times
we use case ... of vs. how many times we use if ... end in our libraries).
The results are then accumulated to give the final result, which will be
output to the screen.

```erlang
%  *---------------*              /\/\/\/\/\/\/\/\/\
%  | erlcount_sup  |             <      ppool       >
%  *---------------*              \/\/\/\/\/\/\/\/\/
%        |                                 |
%        |                                 |
%     erlcount_dispatch              erlcount_counter
```

The structure showed by the diagram is the following:  
 - **ppool** represents the whole application, but only means to show
   that erlcount_counter will be the worker for the process pool.
 - **erlcount_counter** will open files, run the regular expression
   and return the count.
 - **erlcount_sup** process/module will be our supervisor (it will only
   be in charge of erlcount_dispatch)
 - **erlcount_dispatch** will be a single server in charge of browsing
   the directories, asking ppool to schedule workers and compiling the
   results.
 - **erlcount_lib** module (not on the diagram) will take charge of
   hosting all the functions to read directories, compile data and
   whatnot, leaving the other modules with the responsibility of
   coordinating these calls.
 - **erlcount** module (also not on the diagram) is going to be the
   application callback module.

erlcount_dispatch
=================
REQUIREMENTS
 - Only go through the whole list of directories once (when looking
   for .erl files), even when we apply multiple regular expressions
 - We should be able to start scheduling files for result counting
   as soon as we find there's one that matches our criteria. We should
   not need to wait for a complete list to do so.
 - Hold a counter per regular expression so we can compare the results
   in the end
 - It is possible that we start getting results from the **erlcount_counter**
   workers before we're done looking for .erl files
 - It is possible that many erlcount_counters will be running at once
 - It is likely we will keep getting result after we finished looking
   files up in the directories (especially if we have many files or
   complex regular expressions).

How we're going to go through a directory recursively while still being
able to get results from there in order to schedule them, and then accept
results back while that goes on, without getting confused?

1. using a process to do it  
   We would have to add it to the supervision tree and get them to work
   together.

2. [CONTINUATION-PASSING STYLE] (CPS) ---> **erlcount_lib**
   The CPS function is marvellously straight-forward but what to you do with
   the returned continuation? When do you start it back on?
   ANSWER: See **erlcount_dispatch:dispatching/2**

Anyhow, CPS solved, but how are we going to design the dispatcher so
that it can both dispatch and receive at once?

ANSWER: use an FSM with 2 states where both states receive messages from
        ppool all the time.
 - `[-dispatching-]` state used when we are waiting for **find_erl** to
                   hit `done`. While in here, we never think about being
                   done with counting.
 - `[-listening-]` state

```erlang
%     *---------------*            *---------------*
%     |  dispatching  |<--*    *-->|  listening    |
%     *---------------*   |    |   *---------------*
%    /   ^       |        |    |
%   <2> <2>      |       <1>  <1>
%    V   |       |        |    |
%   get_files    |        \    /
%                |         \  /
%           dispatch        \/
%                \           |
%                 \          |
%                  *---> results from
%                        ppool workers
%
% <1> erlcount_dispatch:complete/4
% <2> erlcount_dispatch:dispatching/2
```
Because we are always going async and starting workers with *ppool:async_queue/2* (which is async, returns `noalloc` if process pool is full), we have no
real way of knowing whether we're finished scheduling files or not.
  |
  *-> Timeout? How can you know what a good timeout value could be?
  |
  *-> Mark each worker with a unique ID (ref) and their replies can
      be matched one-on-one. (`-record(data, {regex=[], refs=[]}).`)

WHY ARE WE SENDING MESSAGES TO OURSELVES WHEN WRITING THE FSM?
--------------------------------------------------------------
From LYSE:
> "Remember, because Erlang's behaviours are pretty much all based on
>  messages, we have to do the ugly step of sending ourselves messages if
>  we want to trigger a function call and do things our way."

Once the FSM is up, it is waiting for messages in its main gen_fsm:loop/7,
and without sending messages, nothing is goint to happen. Also, we are
using the FSM internally, the module itself is the best place to send them.

Sending from init/1 is justified as we have to get the ball rolling, and
once init/1 has returned, our FSM will be stuck in the gen_fsm:loop/7
waiting for messages - where should they come from at this point?
```erlang
loop(..., dispatching, ....) ->
    Msg = receive
              Info -> Info
          end,
    decode_msg(Msg,...)

decode_msg(Msg, ...) ->
    ...
    handle_msg(Msg,...)

dispatch(Msg, ...)
```

COULD WE USE `send_event/2` INSTEAD OF `!` FROM `init/1`?
--------------------------------------------------
We could send 
    `gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir))`
but it would place to much burden on init/1. What if there
are no `.erl` files in the dir or the regex is bad?
This is just not the place for this.

On the other hand, why not use `gen_fsm:send_event/2` to
send `{start, Dir}`? It is asynchronous and calling it
> returns ok immediately. The gen_fsm will call Module:StateName/2 to
> handle the event, where StateName is the name of the current state of
> the gen_fsm.
In the and it is a simple `Pid ! {$gen_event, Event}` message, that
will get stored in the MAILBOX, and once `init/1` has finished, it will
be retrieved from the main loop/7.

Basically instead of 
```erlang
%                                                          PROCESS
%  [erlcount_dispatch]                                     MAILBOX                    gen_fsm
%  ===================                                     =======                    =======
%                                                                                     init_it(...)
%  init([]) <---------------------------------------------------------------------------- |
%      ...                                                                                |
%      self ! {start, Dir} ~~~~~~~~~~~~~~~~~~~~~~~~~~~~> {start,Dir}                      |
%      {ok, dispatching, StateData} --------------------------:-------------------------> |
%                                                             :                       loop(..., dispatching, ...) ->
%                                                             :                           Msg = receive
%                                                             *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Info -> Info
%                                                                                               end,
%                                                                                         decode_msg(Msg, ...)
%                                                                                         |
%                                                                                         |
%                                                                                      decode_msg({start,Dir}, ...)
%                                                                                         |
%                                                                                         |
%                                                                                      handle_msg({start,Dir}, ...)
%                                                                                         |
%                                                                                         |
%                                                                                   N  dispatch({'$gen_event', Event}, ...) ->
%                                                                                   N  dispatch({'$gen_all_state_event', Event}, ...) ->
%                                                                                             :
%                                                                                   Y  dispatch(Info = {start,Dir}, ...) ->
%  handle_info({start, Dir}, StateName, StateData) <-------------------------------------- Mod:handle_info(Info, StateName, StateData).
%         |                                                                                   :
%      gen_fsm:send_event(self(), Continuation()) ~~~~~> {'$gen_event', ...}                  :
%         |                                                    :                              :
%      {next_state, dispatching, ...} -------------------------:---------------------> loop(..., dispatching, ...)
%                                                              :                           Msg = receive
%                                                              *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Info -> {'$gen_event', {continue, ...}}
%                                                                                                end,
%                                                                                                 |
%                                                                                                 |
%                                                                                    Y dispatch({'$gen_event', Event}, dispatching, StateData)
%  dispatching({continue, ...) <---------------------------------------------------------- Mod:dispatching(Event)
```

TEST THIS!
we could have written an extra clause for `[-dispatching-]` state:
```erlang
%                                                       PROCESS
%  [erlcount_dispatch]                                  MAILBOX                                 gen_fsm
%  ===================                                  =======                                 =======
%                                                          :                                    init_it(...)
%  init([]) <----------------------------------------------:--------------------------------------- |
%      ...                                                 :                                        |      Name,   Event
%      gen_fsm:send_event(self(), {start, Dir}) -----------:----------------------------------> send_event(self(), {start, Dir})
%          |                                            {'$gen_event,{start, Dir}} <~~~~~~~~~~~~~~~ self() ! {'$gen_event', {start, Dir}},
%          |                                               !                                        ok.
%      {ok, dispatching, StateData} -----------------------!--------------------------------------> |
%                                                          !                                        |
%                                                          !                                    loop(..., dispatching, ...) ->
%                                                          !                                        Msg = receive
%                                                          *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Info -> Info
%                                                                                                         end,
%                                                                                                   decode_msg(Msg, ...)
%                                                                                                   |
%                                                                                                   |
%                                                                                                decode_msg({'$gen_event', {start, Dir}}, ...)
%                                                                                                   |
%                                                                                                   |
%                                                                                                handle_msg({'$gen_event', {start, Dir}}, ...)
%                                                                                                   |
%                                                                                                   |
%                                                                                             Y  dispatch({'$gen_event', Event}, ...) ->
%  dispatching({start,Dir}, StateData) <----------------------------------------------------------- Module:StateName({start, Dir}, StateData)
%    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
%    {next_state, StateName, StateData};
%  dispatching({continue, ...)
%    ...
```

https://github.com/erlang/otp/blob/maint/lib/stdlib/src/gen_fsm.erl)
https://github.com/erlang/otp/blob/maint/lib/stdlib/src/gen.erl
https://github.com/erlang/otp/blob/maint/lib/stdlib/src/proc_lib.erl
https://github.com/erlang/otp/blob/74a95b3d511177a9b35c2b0272b9ca5511b6f750/lib/kernel/src/global.erl


WHY CALL `[-listening-]` STATE EXPLICITYLY FROM `[-dispatching-]` ON `done`?
------------------------------------------------------------------------
As I explained to myself over and over above, using an FSM means that
we are in a specific state waiting for messages. Switching a state the
regular means we assume that more messages are going to come in.
```erlang
%                                                       PROCESS
%  [erlcount_dispatch]                                  MAILBOX                                 gen_fsm
%  ===================                                  =======                                 =======
%                                                                                               dispatch({'$gen_event, Event}, dispatching, ...)
%  dispatching({continue, ...) <------------------------------------------------------------------- Mod:dispatching(Event, StateData)
%      gen_fsm:send_event(self(), Continuation()) ~~~~> {'$gen_event', done}                         :
%           |                                                 :                                      :
%      {next_state, dispatching, ...} ------------------------:-------------------------------> loop(..., dispatching, ...)
%                                                             :                                     receive
%                                                             :                                         Info -> ...
%                                                             :                                                  :
%                                                             :                                                  :
%                                                             *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Info -> {'$gen_event', done}
%                                                                                                   end,
%                                                                                                    |
%                                                                                               dispatch({'$gen_event', done}, dispatching, ...)
%   dispatching(done, ...) <----------------------------------------------------------------------- Mod:dispatching(done, StateData)
%             |                                                                                      :
%       {next_state, listening, Data} -------------------------------------------------------------> |
%                                                                                               loop(..., listening, ...)
%                                                                                                   receive
%                                                                                                       Info -> ...STUCK_HERE
```
The main FSM diagram shows that we are expecting messages with results
from the workers in whatever state we are in BUT if all the workers
are finished and have sent in their results by the time we are about to
switch to `[-listening-]` then we would be stuck where the above diagram
stands above.

```erlang
%        FSM                        WORKERS
%         |                            |
%   [-dispatching-]                    |
%         |                            |
%         | ---add_worker (refs+1)---> |
%         | ---add_worker (refs+1)---> |
%         |                            |
%         | <--results (refs-1) ------ |
%         | <--results (refs-1) ------ |
%         |
%     all_results_in
%         |
%   [-listening-]
%         |
%         ?
```
We are tracking all the workers trough their `ref`s and by the global
`handle_event/3` we remove refs of finished workers. We can check for
`#data{refs=[]}` in `[-listening-]` but if never receive any messages,
`listening(done, ...)` will never be called. Unless we switch explicitly.
```erlang
%                                                       PROCESS
%  [erlcount_dispatch]                                  MAILBOX                                 gen_fsm
%  ===================                                  =======                                 =======
%      {next_state, dispatching, ...} ------------------------:-------------------------------> loop(..., dispatching, ...)
%                                                             :                                     receive
%                                                             *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Info -> {'$gen_event', done}
%                                                                                                   end,
%                                                                                                    |
%                                                                                               dispatch({'$gen_event', done}, dispatching, ...)
%   dispatching(done, ...) <----------------------------------------------------------------------- Mod:dispatching(done, StateData)
%             |                                                                                      :
%       listening(done, Data)                                                                        :
%             ,                                                                                      :
%             V                                                                                      :
%   listening(done, Data)                                                                            :
%       {next_state, listening, Data} -------------------------------------------------------------> |
%                                                                                               loop(..., dispatching, ...)
%                                                                                                   receive
%                                                                                                       Info -> ...
%                                                                                                                :
%                                                                                                                :
%   complete(... last_result ...)                                                                                :
%       gen_fsm:send_all_state_event(...) ~~~~~~~~~~~~> {'$gen_all_state_event', Event} ~~~~~~~~~~~~~~> Info -> last_result
%                                                                                                   end
%                                                                                                    |
%                                                                                               dispatch({'gen_all_state_event', ...last_result....}
%   handle_event(last_result, ...) <--------------------------------------------------------------- Mod:handle_event(last_result, ...)
%       ...                                                                                          :
%       listening(done, NewData)                                                                     :
%             ,                                                                                      :
%             V                                                                                      :
%   listening(done, S = #data{refs=[]})                                                              :
%       ...                                                                                          :
%       {stop, normal, done} ----------------------------------------------------------------------> |
%                                                                                               terminate(...)
```

A more "thorough" flow.
```erlang
%         FSM module    =====                            [gen_fsm]                        [gen]                         [proc_lib]
%   [erlcount_dispatch] = M =                            =========                        =====                         ==========
%   =================== =====                               |                               |                                |                                 =====
%     |                                                     |                               |                                |                 PID of main app = A =
%    start_link/0 --------------------------------> start_link(M,[],[]) ---------> start(gen_fsm, link,                      |                    |     |      =====
%                                                                                        M, [],[])                           |                    |     |
%                                                                                           |                                |                    |     |
%                                                                                  do_spawn(gen_fsm, link, ------------> start_link(gen,          |     |
%                                                                                           M, [], [])                       |      init_it,      V     V                ========
%                                                                                                                            |      [gen_fsm, self(), self(), M, [], []] = Args =
%                                                                                                                            |      infinity,                            ========
%                                                                                                                            |      [])
%                                                                                                                            |
%                                                                                                                  Pid = spawn_opt(gen, init_it, Args, [link]) ----------------------> erlang:spawn_opt(proc_lib, init_p,        (?)
%                                                                                                                   |        |                                                                  |           [self() = Parent, Ancestors, gen, init_it, Args],
%                                                                                                       [1]         *--> sync_wait(Pid, infinity)                                               |           [link])
%                                                                                                                            :                                                                  |
%                                                                                                                            :                                                         proc_lib:init_p(A, Ancestors, gen, init_it, Args)
%                                                                                                                       (..receive        ....)                                                 |
%                                                                                                                       (..    {ack, ...} -> .) <~*                                       (store following stuff in process library:
%                                                                                                                                                 !                                          $ancestors    -> [Parent|Ancestors]
%                                                                                                                                                 !                                          $initial_call -> {M, init, 1}
%                                                                                                                                                 a                                             |
%                                                                                  init_it(gen_fsm, A, A, <---------------------------------------c---------------------------------- erlang:apply(gen, init_it, Args)
%                                                                                           |    M,[],[])                                         k
%                                                                                           |                                                     !
%    init([]) <------------------------------------ init_it(A, A,  self(), <------ init_it2(gen_fsm, A, A,                                        !
%      |                                                    M, [], [])                      self(), M, [], [])                                    !
%      |                                                 :                                                                                        !
%      *~~~~~{ok, StateName, StateData}~~~~~~~~~~~~~~~~~>:~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> init_ack(A, {ok, self()}) ~~~~~*
%                                                        |                                                                   |
%                                                        |                                                                   |
%                                                        | <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ok~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
%                                                loop(A,
%                                                  self(),
%                                                  dispatching,
%                                                  StateData,
%                                                  M, infinity, [])
```


[1] https://en.wikipedia.org/wiki/Continuation-passing_style
