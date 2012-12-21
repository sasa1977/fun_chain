% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2, chain_calls/1]).

parse_transform(Forms, _Options) -> erlang_tree:walk(Forms).
  
chain_calls([Initial | Calls]) -> do_chain_calls(Calls, Initial).

do_chain_calls([], LastResult) -> LastResult;
do_chain_calls([CurrentCall | RemainingCalls], LastResult) ->
  do_chain_calls(RemainingCalls, add_last_argument(CurrentCall, LastResult)).

add_last_argument({call, Line, Fun, Args}, Argument) ->
  {call, Line, Fun, Args ++ [Argument]}.