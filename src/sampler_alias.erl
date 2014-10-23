%% Implement weighted sampling with replacement using the alias method.
%%
%% Sources:
%%   1) http://en.wikipedia.org/wiki/Alias_method
%%   2) http://www.keithschwarz.com/darts-dice-coins/
%%   3) https://hips.seas.harvard.edu/blog/2013/03/03/the-alias-method-efficient-sampling-with-many-discrete-outcomes/
%%

-module(sampler_alias).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% API
-export([start_link/1, stop/1]).
-export([get_weight/2, set_weight/3]).
-export([to_proplist/1]).
-export([draw/1]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(alias, {
          len     :: non_neg_integer(),
          keys    :: array:array(),
          weights :: orddict:orddict(),
          alias   :: array:array(),
          probs   :: array:array()
         }).

-type alias_vectors() :: #alias{}.
-type weight()        :: {atom() , non_neg_integer()}.


%% API

-spec start_link([weight()]) -> {ok, pid()}.
start_link(WeightedList) ->
    start_link(WeightedList, os:timestamp()).


%% @private
%% Internal utility to explicitly seed the random number generator.
-spec start_link([weight()], erlang:timestamp()) -> {ok, pid()}.
start_link(WeightedList, Seed) ->
    gen_server:start_link(?MODULE, [WeightedList, Seed], []).


-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).


-spec get_weight(any(), pid()) -> non_neg_integer() | {error, term()}.
get_weight(Key, Pid) ->
    gen_server:call(Pid, {get_weight, Key}).


-spec set_weight(any(), non_neg_integer(), pid()) -> ok | {error, term()}.
set_weight(Key, Weight, Pid) when Weight >= 0 ->
    gen_server:call(Pid, {set_weight, Key, Weight}).


-spec to_proplist(pid()) -> [weight()] | {error, term()}.
to_proplist(Pid) ->
    gen_server:call(Pid, to_proplist).


-spec draw(pid()) -> atom() | {error, term()}.
draw(Pid) ->
    gen_server:call(Pid, draw).


%% gen_server callbacks

init([WeightedList, Seed]) ->
    _ = random:seed(Seed),
    {ok, make_alias(WeightedList)}.


handle_call({get_weight, Weight}, _From, Alias) ->
    {reply, get(weight, Weight, Alias), Alias};

handle_call({set_weight, Key, Weight}, _From, Alias) ->
    UpdatedAlias = set(weight, Key, Weight, Alias),
    {reply, ok, UpdatedAlias};

handle_call(draw, _From, Alias) ->
    {reply, select(Alias), Alias};

handle_call(to_proplist, _From, Alias = #alias{weights = Weights}) ->
    {reply, orddict:to_list(Weights), Alias};

handle_call(stop, _From, Alias) ->
    {stop, normal, Alias};

handle_call(_Other, _Form, Alias) ->
    error_logger:warning_msg("received unknown request: ~p", [_Other]),
    {noreply, Alias}.


handle_cast(_Unknown, Alias) ->
    error_logger:warning_msg("received unknown request: ~p", [_Unknown]),
    {noreply, Alias}.


handle_info(_Unknown, Alias) ->
    error_logger:warning_msg("received unknown message: ~p", [_Unknown]),
    {noreply, Alias}.


terminate(_Reason, _Alias) ->
    ok.


code_change(_OldVsn, Alias, _Extra) ->
    {ok, Alias}.


%% Internal

-spec make_alias([weight()]) -> alias_vectors().
make_alias(WeightedList) ->
    WeightDict = orddict:from_list(WeightedList),
    N = orddict:size(WeightDict),
    SumWeights = orddict:fold(fun (_, V, Acc) -> Acc + V end, 0, WeightDict),

    %% Calculate Probabilites & Normalise by the average (multiply by N).
    ScaledProbabilties = if SumWeights > 0 ->
                                 orddict:fold(fun (_, V, Acc) ->
                                                      Acc ++ [(V/SumWeights) * N]
                                              end, [], WeightDict);
                            true ->
                                 %% Prevent non positive sums to proceed.
                                 error(badarg)
                         end,

    %% Sort probabilities into larger & smaller than 1/N.
    %% Note we already multipilied by N, therfore compare to > or < 1.
    Sort = fun (P, {Small, Large, Index}) when P < 1 ->
                   {[Index | Small], Large, Index + 1};
               (_P, {Small, Large, Index}) ->
                   {Small, [Index | Large], Index + 1}
           end,
    {Small, Large, _} = lists:foldl(Sort, {[],[],0}, ScaledProbabilties),

    %% Itirate and allocate the large probabilites to the low.
    {Alias, Probabilities} = update_probabilities(Small, Large, array:new(N),
                                                  array:from_list(ScaledProbabilties)),

    #alias{len = N,
           keys = array:from_list(orddict:fetch_keys(WeightDict)),
           weights = WeightDict,
           alias = Alias,
           probs = Probabilities}.


get(weight, Key, #alias{weights = Weights}) ->
    orddict:fetch(Key, Weights).


set(weight, Key, Weight, #alias{weights = Weights}) ->
    make_alias(orddict:store(Key, Weight, Weights)).


update_probabilities([], _Large, A, Pr) ->
    {A, Pr};
update_probabilities(_Small, [], A, Pr) ->
    {A, Pr};
update_probabilities([S | Small], [L | Large], A, Pr) ->
    %% Fill the small
    A1 = array:set(S, L, A),
    %% Calculate replacement probability.
    P = array:get(L, Pr) - (1 - array:get(S, Pr)),
    Pr1 = array:set(L, P, Pr),
    {Small1, Large1} = case P < 1 of
                           true -> {[L | Small], Large};
                           false -> {Small, [L | Large]}
                       end,
    update_probabilities(Small1, Large1, A1, Pr1).


-spec select(alias_vectors()) -> any().
select(#alias{len = N, keys = K, alias = A, probs = Pr}) ->
    %% Role the die
    DieRoll = random:uniform(N) - 1,
    %% Now toss the coin
    Random = random:uniform(),
    Index = case Random =< array:get(DieRoll, Pr) of
                true ->
                    DieRoll;
                false ->
                    array:get(DieRoll, A)
            end,

    array:get(Index, K).

-ifdef(TEST).

-define(SAMPLE, 10000).

create_and_sample_results(WeightedList) ->
    {ok, Sampler} = sampler_alias:start_link(WeightedList),
    draw_n_samples(Sampler, ?SAMPLE).


never_never_occurs_test() ->
    Probabilities = create_and_sample_results([{always, 1}, {never, 0}]),

    %% Ensure never is not set.
    ?assertEqual(false, proplists:is_defined(never, Probabilities)),

    ?assertEqual(1.0, proplists:get_value(always, Probabilities)).


simple_weighted_test() ->
    Probabilities = create_and_sample_results([{twice, 2}, {once, 1}, {never, 0}]),

    %% Ensure never is not set.
    ?assertEqual(false, proplists:is_defined(never, Probabilities)),

    %% Check Twice to be about 66% multiplying 10 & trunc reduces
    %% floating point precision.
    Twice = proplists:get_value(twice, Probabilities, 0),
    ?assertEqual(6, trunc(Twice *10)),

    %% Check once
    Once = proplists:get_value(once, Probabilities, 0),
    ?assertEqual(3, trunc(Once * 10)).


equal_distribution_test() ->
    Probabilities = create_and_sample_results([{beer, 1},
                                               {code, 1},
                                               {food, 1},
                                               {sleep, 1}]),
    [?_assertEqual(0.25, P) || {_, P} <- Probabilities].


update_and_find_weight_test() ->
    %% Simple Validation Tests
    Alias = make_alias([{one, 10}, {two, 20}, {three, 30}]),
    ?assertEqual(10, get(weight, one, Alias)),
    ?assertEqual(20, get(weight, two, Alias)),
    ?assertEqual(30, get(weight, three, Alias)),

    Alias1 = set(weight, two, 200, Alias),
    ?assertEqual(10, get(weight, one, Alias1)),
    ?assertEqual(200, get(weight, two, Alias1)),
    ?assertEqual(30, get(weight, three, Alias1)).


run_proper_test_() ->
    { %% Timeout proper after 5 min
      timeout, 360,
      ?_assertEqual([], proper:module(sampler_alias, [{to_file, user}]))
    }.


%% draw n times
draw_n_samples(Pid, N) ->
    Samples = [draw(Pid) || _ <- lists:seq(1, N)],
    Aggregate = lists:foldl(
                  fun(V, D) -> dict:update_counter(V, 1, D) end,
                  dict:new(),
                  Samples),
    %% Calculate Probabilities
    [{K, V / N} || {K, V} <- dict:to_list(Aggregate)].


draw_and_check(Weights, Seed) ->
    {ok, Sampler} = start_link(Weights, Seed),
    NoDuplicateWeights = orddict:from_list(Weights),
    Probabilities = draw_n_samples(Sampler, ?SAMPLE),
    SumWeights = lists:foldl(fun ({_, W}, Acc) ->  W + Acc end,
                             0, NoDuplicateWeights),

    Test = fun (Key, Probability) ->
                   Pr = proplists:get_value(Key, NoDuplicateWeights) / SumWeights,
                   %% Check accuracy to 0.02
                   abs(Pr - Probability) < 0.02
           end,

    [{K, P, Test(K, P)} || {K, P} <- Probabilities].


prop_sample() ->
    %% Ensure we always have a positive sum of weights.
    ?FORALL({Weights, Seed}, {non_empty(list({atom(), pos_integer()})), erlang:timestamp()},
           begin
               Probs = draw_and_check(Weights, Seed),
               Check = fun ({_, _, Test}, Acc) -> Test and Acc end,
               ?WHENFAIL(erlang:display({"Error Probabilities", Probs}),
                         lists:foldl(Check, true, Probs))
           end).


prop_set_weight() ->
    {ok, Pid} = sampler_alias:start_link([{foo, 1}]),
    ?FORALL(Weight, weight(),
            begin
                {Key, Value} = Weight,
                ok = sampler_alias:set_weight(Key, Value, Pid),
                Value =:= sampler_alias:get_weight(Key, Pid)
            end).

-endif.
