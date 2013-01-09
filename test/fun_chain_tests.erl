-module(fun_chain_tests).
-include_lib("eunit/include/eunit.hrl").

chain_last_test_() -> 
  [
    ?_assertEqual(dict:new(), compile_dynamic("fun_chain:last(dict:new())")),
      
    ?_assertEqual(
      dict:from_list([{b,2}, {c,3}]),
      compile_dynamic("
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
      compile_dynamic("fun_chain:last(fun_chain:last(dict:new()))")
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      compile_dynamic("
        fun_chain:last(dict:new(),
          (fun(Dict) ->
            dict:store(a,1,Dict)
          end)()
        )
      ")
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      compile_dynamic("
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
    ?_assertEqual(9, compile_dynamic("
      Inc = fun(X, Increment) -> X + Increment end,
      fun_chain:first(1, Inc(3), Inc(5))
    "))
  ].

% Dynamic execution of the code. See dynamic_generator for explanation.
compile_dynamic(Code) ->
  dynamic_generator:exec_dynamic(dynamic_test,
    "-compile({parse_transform, fun_chain}).",
    Code
  ).