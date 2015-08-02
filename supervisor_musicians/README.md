http://learnyousomeerlang.com/supervisors

LYSE supervisor example with managers and bands, updated with the new
supervisor convention to use maps instead of records when defining
supervisors.

The example is also dumbed down to my level and added probably superfluous
comments stating the obvious.

```erlang band_supervisor:start_link(Type).
% With the current setup, the band members will start playing in
% the following order:
%
%    (1) singer - good (permanent)
%    (2) bass   - good (transient)
%    (3) drum   - bad  (transient)
%    (4) keytar - good (temporary)
```
The outcomes:
 - lenient [one_for_one]: The drummers can stumble 3 times (each drummer
 once) in 1 minute otherwise they piss off the manager and everyone
 leaves. ("Aztan jott a csosz, es mindenkit kizavart az erdobol.")
 - angry   [rest_for_one]: At the first drummer fuckup the drummer and
 the keytar player are let go, a new drummer will be hired and the
 quartet continues as a trio. (The keytar is lame anyway.) 
 - jerk [one_for_all]: If the drummer messes up, the whole band is fired,
 and a new lineup is hired. Should the new drummer miss a beat in a 1
 minute period with the last failure, everyone will be kicked out and
 the manager jumps off a bridge.

You can play with reconfiguring the child specs of the various band
members a see how it affects the outcome. Try putting in more bad
musicians for example.

Check out the other functions as well (caveat: not all of them work with simple_one_for_one)

*start_child(SupervisorNameOrPid, ChildSpec)*
This adds a child specification to the list and starts the child with it

*terminate_child(SupervisorNameOrPid, ChildId)*
Terminates or brutal_kills the child. The child specification is left in the supervisor

*restart_child(SupervisorNameOrPid, ChildId)*
Uses the child specification to get things rolling.

*delete_child(SupervisorNameOrPid, ChildId)*
Gets rid of the ChildSpec of the specified child

*check_childspecs([ChildSpec])*
Makes sure a child specification is valid. You can use this to try it before using 'start_child/2'.

*count_children(SupervisorNameOrPid)*
Counts all the children under the supervisor and gives you a little comparative list of who's active, how many specs there are, how many are supervisors and how many are workers.

*which_children(SupervisorNameOrPid)*
gives you a list of all the children under the supervisor.

TODO: test simple_one_for_one too at one point.
