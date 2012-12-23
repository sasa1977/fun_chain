%% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2]).

parse_transform(ParseTree, _Options) -> deep_walk(ParseTree).

deep_walk(List) when is_list(List) ->
  [deep_walk(Element) || Element <- List];

deep_walk({call, _Line, {remote, _, {atom, _, fun_chain}, {atom, _, last}}, [Initial | Calls]}) ->
  chain_calls(last, Calls, deep_walk(Initial));

deep_walk(Tuple) when is_tuple(Tuple) ->
  list_to_tuple([deep_walk(Element) || Element <- tuple_to_list(Tuple)]);

deep_walk(Other) -> Other.
  
chain_calls(_, [], LastResult) -> LastResult;

chain_calls(Where, [CurrentCall | RemainingCalls], LastResult) ->
  chain_calls(Where, RemainingCalls, add_argument(Where, CurrentCall, LastResult)).

add_argument(last, {call, Line, Fun, Args}, Argument) ->
  {call, Line, deep_walk(Fun), deep_walk(Args) ++ [Argument]}.