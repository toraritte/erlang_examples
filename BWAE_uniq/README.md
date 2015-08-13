Building Web Applications with Erlang by Zachary Kessin (O’Reilly).  
Copyright 2012 Zachary Kessin, ISBN 978-1-449-30996-1

### Sending a message to a gen* behavior (no `handle_info/2`)
> "... you could omit the `handle_info/2` function
> altogether; you will get a warning when you
> compile but it can be ignored. In this case,
> when an unknown message is sent to the process
> `gen_server` will terminate the process and leave
> an error message. In general, this is what you
> want as the supervisor will recreate it."

```erlang
% 83> c(uniq_serv), c(uniq_sup).
% uniq_serv.erl:2: Warning: undefined callback function handle_info/2 (behaviour 'gen_server')
% {ok,uniq_sup}
% 84> uniq_sup:start_link().
% {ok,<0.209.0>}
% 85> 
% 85> plinks([uniq_sup, uniq_serv]).
%    <0.209.0>   uniq_sup    links to [<0.197.0>,uniq_serv]
%    <0.210.0>   uniq_serv   links to [uniq_sup]
%    ok
% 86> [uniq_serv:get_id() || _ <- lists:seq(1,7)].
% [1,2,3,4,5,6,7]
% 87> 
% 87> uniq_serv ! lofa.
% lofa
% 88> 
% =ERROR REPORT==== 13-Aug-2015::15:55:44 ===
% (...)
% 88> plinks([uniq_sup, uniq_serv]).              
%    <0.209.0>   uniq_sup    links to [<0.197.0>,uniq_serv]
%    <0.215.0>   uniq_serv   links to [uniq_sup]
%    ok
% 89> [uniq_serv:get_id() || _ <- lists:seq(1,7)].
% [1,2,3,4,5,6,7]
```
Which brings us to the next point.

### How do you keep the state of `gen_server` after a crash?
http://stackoverflow.com/questions/13199528/restarting-erlang-process-and-preserving-state

In this case keeping the state is not really relevant because
`uniq_serv:get_id/0` provides unique ids but this is not the best
approach. Erlang stdlib is full of useful functions to always get a
unique identifier.

What would be a valid example to this?
