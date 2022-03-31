{-

Garrett Hill
CMSC488B
Spring 2022

-}

module TargetedPBT where
import Test.QuickCheck
import Data.Graph as Graphs


sut = undefined


prop_length :: Graph g -> Property
prop_length g =
  counterexample "wrong length!" all lenLam (vertices g)
  where
    lenLam = \v -> distanceFromSink v < 21
    distanceFromSink = undefined

prop_length_hc :: Graph g -> Property
prop_length_hc = 


{- Code to reimplement

prop_Target() -> % Try to check a property
  ?TARGET_STRATEGY(SearchStrategy, % for some Search Strategy
    ?FORALL(Input, ?TARGET(Params), % and for some Parameters
            begin % for the input generation.
              UV = SUT:run(Input), % Do so by running SUT with Input
              ?MAXIMIZE(UV), % and maximize its Utility Value
              UV < Threshold % up to some Threshold.
            end)).

prop_length_hc() ->
  ?TARGET_STRATEGY(hill_climbing,
    ?FORALL(X, ?TARGET(graph_hc(42)),
            begin
              UV = lists:max(distance_from_sink(G)),
              ?MAXIMIZE(UV),
              UV < 21
            end))

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
