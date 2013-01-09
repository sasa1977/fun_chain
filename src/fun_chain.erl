%% The fun_chain parse transform. See README.md for usage

-module(fun_chain).
-export([parse_transform/2]).

parse_transform(ParseTree, _Options) -> deep_walk(ParseTree).

% Top level function -> here we have to report error if we have any.
deep_walk({function, _, _, _, _} = FunctionDeclaration) ->
  result_or_error(deep_walk_tuple(FunctionDeclaration));

% fun_chain call
deep_walk({call, Line, {remote, _, {atom, _, fun_chain}, {atom, _, ArgumentPosition}}, Clauses}) ->
  chain_calls(Line, ArgumentPosition, Clauses);

deep_walk(List) when is_list(List) -> deep_walk_list(List);
deep_walk(Tuple) when is_tuple(Tuple) -> deep_walk_tuple(Tuple);
deep_walk(Other) -> Other.


% Walking through tuples and lists. Each subvalue must be inspected for errors. If an error
% has occured, it is propagated up the stack
deep_walk_tuple(Tuple) ->
  parse_chain(deep_walk_list(tuple_to_list(Tuple)), fun erlang:list_to_tuple/1).

deep_walk_list(List) -> 
  parse_chain(deep_walk_list(List, []), fun lists:reverse/1).

deep_walk_list([], Acc) -> Acc;
deep_walk_list([H|T], Acc) ->
  parse_chain(deep_walk(H), fun(Result) ->
    deep_walk_list(T, [Result | Acc])
  end).


% Chaining of function calls.
chain_calls(Line, _, []) -> make_parse_error(Line, "clauses are empty");

chain_calls(_, ArgumentPosition, [Initial | Rest]) 
  when (ArgumentPosition =:= first orelse ArgumentPosition =:= last) -> 
  do_chain_calls(ArgumentPosition, Rest, deep_walk(Initial));
  
chain_calls(Line, ArgumentPosition, _) ->
  make_parse_error(Line, io_lib:format("invalid function ~p", [ArgumentPosition])).


do_chain_calls(_, [], LastResult) -> LastResult;

do_chain_calls(ArgumentPosition, [CurrentCall | RemainingCalls], LastResult) ->
  parse_chain(
    add_argument(ArgumentPosition, CurrentCall, LastResult), 
    fun(Args) ->
      do_chain_calls(ArgumentPosition, RemainingCalls, Args)
    end
  ).

% Appending of arguments to appropriate place.
add_argument(last, {call, Line, Fun, Args}, Argument) ->
  {call, Line, deep_walk(Fun), deep_walk(Args) ++ [Argument]};
  
add_argument(first, {call, Line, Fun, Args}, Argument) ->
  {call, Line, deep_walk(Fun), [Argument | deep_walk(Args)]};
  
add_argument(_, Term, _) -> make_parse_error(element(2, Term), "not a function call").


% Maybe monad style helper which chains two functions. If first function returns parse error, the second
% one is not invoked.
parse_chain({parse_error, _} = Error, _) -> Error;
parse_chain(Result, Fun) -> Fun(Result).

result_or_error({parse_error, Error}) -> Error;
result_or_error(Result) -> Result.

% Wraps parse error in a custom tuple which can be safely propagated up the call stack.
make_parse_error(Line, Message) ->
  {parse_error, 
    {error,
      {Line, erl_parse,[io_lib:format("fun_chain error: ~s", [Message])]}
    }
  }.