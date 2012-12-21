% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2, handle_chain/1]).

parse_transform(Forms, _Options) ->
  id_trans_hooks:parse_transform(Forms, _Options). % start standard parser
  
%% Start fun_chain parsing.
handle_chain([Initial | Rest]) ->
  chain_call(Rest, Initial).

%% Recursive handling of the chain call.
chain_call([], Acc) -> Acc; % last call in chain -> return Acc
chain_call([{call,Line,F0,As0} | Rest], Acc) ->
  % Add the call to the list of arguments of the next call
  chain_call(Rest, {call, Line, F0, As0 ++ [Acc]}).
  