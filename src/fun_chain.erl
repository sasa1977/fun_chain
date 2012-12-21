%% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2]).

parse_transform(ParseTree, _Options) -> deep_walk(ParseTree).

deep_walk(List) when is_list(List) ->
  [deep_walk(Element) || Element <- List];

deep_walk({call, _Line, {atom, _, fun_chain}, [Initial | Calls]}) ->
  chain_calls(Calls, Initial);

deep_walk(Tuple) when is_tuple(Tuple) ->
  list_to_tuple([deep_walk(Element) || Element <- tuple_to_list(Tuple)]);

deep_walk(Other) -> Other.
  
chain_calls([], LastResult) -> LastResult;

chain_calls([CurrentCall | RemainingCalls], LastResult) ->
  chain_calls(RemainingCalls, add_last_argument(CurrentCall, LastResult)).

add_last_argument({call, Line, Fun, Args}, Argument) ->
  {call, Line, Fun, Args ++ [Argument]}.
  
