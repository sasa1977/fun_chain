-module(fun_chain).
-export([parse_transform/2, handle_chain/1]).

parse_transform(Forms, _Options) ->
  id_trans_hooks:parse_transform(Forms, _Options).
  
handle_chain([Initial | Rest]) ->
  chain_call(Rest, Initial).
  
chain_call([], Acc) -> Acc;
chain_call([{call,Line,F0,As0} | Rest], Acc) ->
  chain_call(Rest, {call, Line, F0, As0 ++ [Acc]}).
  