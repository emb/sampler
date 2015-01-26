# README

This is an erlang application that will potentially provide various
methods of sampling a population.

## Current Status

Provide sampling using the
[Alias Method](http://en.wikipedia.org/wiki/Alias_method).

### Example

```
emb@kimba:~/wa/sampler$ erl -pa ebin/
Erlang/OTP 17 [erts-6.2] [source] [smp:2:2] [async-threads:10] [kernel-poll:false] 

Eshell V6.2  (abort with ^G)
1> 
1> %% Initialize some pool with equal weights.
1> {ok, S} = sampler_alias:start_link([{db_conn1, 10}, {db_conn2, 10}]).
{ok,<0.35.0>}
2>
2> %% Helper method to display the weights.
2> sampler_alias:to_proplist(S).
[{db_conn1,10},{db_conn2,10}]
3>
3> %% Draw a member of the pool. This should follow a random
3> %% distribution
3> sampler_alias:draw(S).
db_conn2
4> sampler_alias:draw(S).
db_conn1
5> sampler_alias:draw(S).
db_conn1
6> sampler_alias:draw(S).
db_conn2
7> sampler_alias:draw(S).
db_conn1
8> sampler_alias:draw(S).
db_conn1
9> sampler_alias:draw(S).
db_conn2
10> sampler_alias:draw(S).
db_conn2
11> sampler_alias:draw(S).
db_conn1
12> sampler_alias:draw(S).
db_conn2
13>
13> %% Update the weight.
13> sampler_alias:set_weight(db_conn1, 1, S).
ok
14> sampler_alias:to_proplist(S).            
[{db_conn1,1},{db_conn2,10}]
15>
15> %% Notice drawing db_conn1 will be quite rare.
15> sampler_alias:draw(S).                   
db_conn2
16> sampler_alias:draw(S).
db_conn2
17> sampler_alias:draw(S).
db_conn2
18> sampler_alias:draw(S).
db_conn2
19> 
```

## Application

The alias method is quite useful to create a weighted pool of
connections. If a single connection goes bad or flaky, reducing its
weight in the list of weights will reduce the likelyhood of the
connection being drawn.
