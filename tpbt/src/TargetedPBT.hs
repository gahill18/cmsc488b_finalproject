{-

Garrett Hill
CMSC488B
Spring 2022

-}

module TargetedPBT where

import Data.List

{-
Takes in a function to calculate uvs, a function to calculate a set of nearby points from a given point, and an initial guess point.
Returns (local maximum, corresponding uv)
-}
maximize :: Ord b => (a -> b) -> (a -> [a]) -> a -> (a, b)
maximize f_uv f_wiggle init =
  let is = f_wiggle init
      uvs = map f_uv is
      l = zip is uvs
  in tupleMax l

{-
Takes in a list of tuples, of which the second type needs to be orderable in order to sort.
Returns the tuple with the largest second value.
-}
tupleMax :: Ord b => [(a, b)] -> (a, b)
tupleMax [] = error "tupleMax passed empty list!"
tupleMax [x] = x
tupleMax ((a,b):t) = let (a',b') = tupleMax t in
  if b < b' then (a',b')
  else (a,b)


{-
Takes in a function to calculate uvs, a function to calculate a set of nearby points from a given point, and an initial guess point.
Returns (local minimum, corresponding uv)
-}
minimize :: Ord b => (a -> b) -> (a -> [a]) -> a -> (a, b)
minimize f_uv f_wiggle init =
  let is = f_wiggle init
      uvs = map f_uv is
      l = zip is uvs
  in tupleMin l

{-
Takes in a list of tuples, of which the second type needs to be orderable in order to sort.
Returns the tuple with the lowest second value.
-}
tupleMin :: Ord b => [(a, b)] -> (a, b)
tupleMin [] = error "tupleMin passed empty list!"
tupleMin [x] = x
tupleMin ((a,b):t) = let (a',b') = tupleMin t in
  if b > b' then (a',b')
  else (a,b)

sampleUV :: Int -> Int
sampleUV i = (10 - i) ^ 2

-- Wiggle is a neighborhood function
-- It produces a series of 'nearby 'data points
sampleWiggle :: Int -> [Int]
sampleWiggle i = [(i - 10) .. (i + 10)]

{-
Takes a uv function, a wiggle function, an initial guess, and a gas level
(number of steps before termination).
Returns (a local minimum, associated uv value)
-}
gradientDescent :: Ord b => (a -> b) -> (a -> [a]) -> a -> Int -> (a, b)
gradientDescent f_uv f_wiggle i 0 = (i, f_uv i)
gradientDescent f_uv f_wiggle i gas =
  let (new_i,new_uv) = minimize f_uv f_wiggle i in
    if new_uv == f_uv i then (new_i,new_uv)
    else gradientDescent f_uv f_wiggle new_i (gas - 1)

sampleGD i g = gradientDescent sampleUV sampleWiggle i g

{-
consider doing decision trees on binary function to
determine how to beat wordle

experiment with expanding gradient descent to accept functions
with unlimited arguments

roadmap:
implement typeclass to demonstrate properties
- need to include utility function and neighborhood function
- implement arbitrary so that you can generate instances of the objects
- for every test that quickcheck generates an arbitrary instance for, gradientDescent to check nearby points for higher likelhihood to fail
-}

{- Code to reimplement


prop_length_hc() ->
  ?TARGET_STRATEGY(hill_climbing,
    ?FORALL(X, ?TARGET(graph_hc(42)),
            begin
              UV = lists:max(distance_from_sink(G)),
              ?MAXIMIZE(UV),
              UV < 21
            end))

prop_Target() -> % Try to check a property
  ?TARGET_STRATEGY(SearchStrategy, % for some Search Strategy
    ?FORALL(Input, ?TARGET(Params), % and for some Parameters
            begin % for the input generation.
              UV = SUT:run(Input), % Do so by running SUT with Input
              ?MAXIMIZE(UV), % and maximize its Utility Value
              UV < Threshold % up to some Threshold.
            end)).


graph_hc(N) ->
  #{first => graph(N), next => fun graph_next/1}

-define(TARGET(Params), targeted(make_ref(), Params)).

targeted(Key, Params) ->
  ?LAZY(targeted_gen(Key, Params)).

targeted_gen(Key, Params) ->
  {State, NextFunc, _UpdateFunc} = get_target(Key, Params),
  {NewState, NextValue} = NextFunc(State),
  update_target(Key, NewState),
  NextValue

-define(MAXIMIZE(UV), update_target_uvs(UV)).

update_target_uvs(UV) ->
  [update_target_uv(Key, UV) || Key <- get_target_keys()].

update_target_uv(Key, UV) ->
  {State, _NextFunc, UpdateFunc} = get_target(Key, []),
  NewState = UpdateFunc(State, UV),
  update_target(Key, NewState)



---------- Hill climbing strategy ---------

init_target(#{first := First, next := Next}) ->
  { %% 1st element: initial state
    {generate_sample(First), unknown, none},
    %% 2nd element: next function
    fun ({LastAcc, AccUtility, _LastGen}) ->
      NewValue = generate_sample(Next(LastAcc)),
      {{LastAcc, AccUtility, NewValue}, NewValue}
    end,
    %% 3rd element: state-update function
    fun ({LastAcc, AccUtility, LastGen}, GenUtility) ->
      case AccUtility =:= unknown orelse GenUtility > AccUtility of
        true -> % accept new solution
          {LastGen, GenUtility, LastGen};
        false -> % continue with old solution
          {LastAcc, AccUtility, LastGen}
      end
    end
  }


----------- Neighborhood function for HC ----------

graph_next(G) ->
  Size = graph_size(G),
  ?LET(NewSize, neighboring_integer(Size),
       ?LET(Additional, neighboring_integer(Size div 10),
            begin
              {Removals, Additions} =
                case NewSize < Size of
                  true -> {Additional + (Size - NewSize), Additional};
                  false -> {Additional, Additional + (NewSize - Size)}
                end,
              ?LET(G_Del, remove_n_edges(G, Removals),
                   add_n_edges(G_Del, Additions))
            end)).

graph_size({_, E}) ->
  length(E).

%% generator for neighboring integer
neighboring_integer(Base) ->
  Offset = trunc(0.05 * Base) + 1,
  ?LET(X, proper_types:integer(Base - Offset, Base + Offset), max(0,X)).

add_n_edges({V, E}, N) ->
  ?LET(NewEdges, proper_types:vector(N, edge(V)),
     {V, lists:usort(E ++ NewEdges)}).

remove_n_edges({V, E}, 0) -> {V, E};
remove_n_edges({V, []}, _) -> {V, []};
remove_n_edges({V, E}, N) ->
  ?LET(Edge, proper_types:oneof(E),
    ?LAZY(remove_n_edges({V, lists:delete(Edge, E)}, N - 1)))


integer(Low, High) ->
  #{first => proper_types:integer(Low, High),
    next => integer_next(Low, High)}.

integer_next(Low, High) ->
  fun (OldInstance, Temperature) ->
    Offset = trunc(abs(Low - High) * Temperature * 0.1) + 1,
    ?LET(X, proper_types:integer(-Offset, Offset),
         ensure_range(X + OldInstance, Low, High))
  end.

graph_sa(N) ->
  #{first => graph(N), next => fun (Base, _T) -> graph_next(Base) end}.


-}
