
http://learnyousomeerlang.com/building-applications-with-otp

```erlang
%            ppool_supersup
%                 |        \
%                 |  (...)  *-----------------------------------*
%                 |                                              \
%  *---pool_1-----|------------------------------*    (...)   *---pool_N---*
%  |              |                              |            |            |
%  |         ppool_sup                           |            |            |
%  |         /       \                           |            *------------*
%  |        /         \                          |
%  |   ppool_serv    worker_sup                  |
%  |   (gen_server) /  |       \                 |
%  |               /   | (...)  \                |
%  |              /    |         \               |
%  |         worker1  worker2   workerN          |
%  |                                             |
%  *---------------------------------------------*
```
We use one gen_server per pool. The server's job is to  
 - maintain the counter of how many workers are in the pool
 - hold the queue of tasks
 - track the processes to count them
 - supervising the processes itself

Neither the server nor the processes can crash without losing the
state of all the others (otherwise the server can't track the tasks
after it restarted). Using a server has a few disadvantages too: it has
many responsibilities, can be seen as more fragile and duplicates the
functionality of existing, better tested modules.

ppool_supersup
--------------
The top level supervisor, starting without any children. Starting new
children (new pools) as necessary.

ppool_sup
---------
Starts up ppool_serv ONLY, worker_sup will be started by ppool_serv for
reasons described below.

ppool_serv
----------
Operations to support:
1. **run/2**
   Run a task in the pool and send notification if it can't be started
   because the pool is full.
2. **sync_queue/2**
   Run a task in the pool or if it's full, keep the calling process
   waiting while the task is in the queue, until it can be run.
3. **async_queue/2**
   Run a task asynchronously in the pool, as soon as possible. If no
   place is available, queue it up and run it whenever.

The ppool_serv should be able to contact the worker_sup process because it
needs to track the processes of the pool, but according to the diagram
above if we start them at the same time then it is pretty tricky to do that.

The way to do it is basically to get the pool server to dynamically
attach the worker supervisor to its ppool_sup: **ppool_sup** starts up
(and passes its PID to) **ppool_serv**.

This will let the server call for the spawning of the worker supervisor;
the MFA variable will be used in that call to let the simple_one_for_one
supervisor know what kind of workers to run.

This is how the above diagram looks like in real life:

```erlang
%  *---pool_1-----|---------------*
%  |              |               |
%  |         ppool_sup            |
%  |        /   >    \            |
%  |      (1)  /     (3)          |
%  |      /  (2)       >          |
%  |     <   /    *-> worker_sup   |
%  | ppool_serv  /                |
%  |            /                 |
%  *-----------/------------------*
%             /
%  *->----->-*
%  |
%  | ppool_sup <------------------------<ppool_serv:handle_info/2 <-----------------*
%  |      |                                    :                                    |
%  |      | ---------spawn--------------> ppool_serv                                |
%  |      |                                    |                                    |
%  |   (waits for                           init/1                                  |
%  |    ppool_serv:init/1)                     |                                    |
%  |      |                                    | ----- self() ! start_worker_sup ---*
%  |      |                                    |
%  |      * <---------{ok,State}----------<--- *
%  *--<-- |
```

The straight-forward solution below will not work because it causes
a deadlock. The way things work with gen_* behaviours is that the
process that spawns the behaviour waits until the init/1 function
returns before resuming its processing. This means that if we call
supervisor:start_child/2 in ppool_serv:init/1 then:

```erlang
init({Limit, MFA, Sup}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {ok, SomeState}.

% ppool_sup
%      |
%      | ---------- spawn -----------> ppool_serv
%      |                                    |
% (waiting for                            init/1
%  {ok, SomeState})                         |
%      |                                    |
%      | <------- start_child ------------- |
%      |                                    |
%      |                             (waiting for start_child to
%      |                              return with {ok, Pid})
%      |                                    |
%      :                                    :
```

**See diagram in LYSE_erlcount/README.md**
[1] on diagram:
    The main process (that spawns the behaviour) is synchronous and  *explicitly* waiting
    for 2 messages after spawning:
       {ack, ...}
           or
       {'EXIT', ...}
    `supervisor:start_child/2` is also synchronous and it shoots off its message and
    then it will wait for {ok, Pid}. BAM!

