% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2, chain_calls/1]).

parse_transform(Forms, _Options) -> erlang_tree:walk(Forms).
  
chain_calls([Initial | Rest]) -> do_chain_calls(Rest, Initial).

do_chain_calls([], LastResult) -> LastResult;
do_chain_calls([{call, Line, Fun, Args} | Rest], LastResult) ->
  do_chain_calls(Rest, {call, Line, Fun, Args ++ [LastResult]}).
  