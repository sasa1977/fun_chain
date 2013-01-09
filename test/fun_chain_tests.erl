-module(fun_chain_tests).
-include_lib("eunit/include/eunit.hrl").

chain_last_test_() -> 
  [
    ?_assertEqual(dict:new(), exec_dynamic("fun_chain:last(dict:new())")),
      
    ?_assertEqual(
      dict:from_list([{b,2}, {c,3}]),
      exec_dynamic("
        fun_chain:last(dict:new(),
          dict:store(a,1),
          dict:store(b,2),
          dict:erase(a),
          dict:store(c,3)
        )
      ")
    ),
    
    ?_assertEqual(
      dict:new(), 
      exec_dynamic("fun_chain:last(fun_chain:last(dict:new()))")
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      exec_dynamic("
        fun_chain:last(dict:new(),
          (fun(Dict) ->
            dict:store(a,1,Dict)
          end)()
        )
      ")
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      exec_dynamic("
        fun_chain:last(dict:new(),
          (fun(Dict) ->
            fun_chain:last(Dict, dict:store(a,1))
          end)()
        )
      ")
    )
  ].
  
  
chain_first_test_() -> 
  [
    ?_assertEqual(9, exec_dynamic("
      Inc = fun(X, Increment) -> X + Increment end,
      fun_chain:first(1, Inc(3), Inc(5))
    "))
  ].
  
error_test_() -> [
  ?_assertMatch({error, _, _}, compile("fun_chain:last()")),
  ?_assertMatch({error, _, _}, compile("fun_chain:invalid_function(3)")),
  ?_assertMatch({error, _, _}, compile("fun_chain:last(3,3)"))
].

% Dynamic execution of the code. See dynamic_generator for explanation.
exec_dynamic(Code) ->
  dynamic_generator:exec_dynamic(dynamic_test,
    "-compile({parse_transform, fun_chain}).",
    Code
  ).
  
compile(Code) ->
  dynamic_generator:compile(dynamic_test,
    "-compile({parse_transform, fun_chain}).",
    Code,
    fun(Result) -> Result end
  ).