-module(fun_chain_tests).
-compile({parse_transform, fun_chain}).

-include_lib("eunit/include/eunit.hrl").

chain_last_test_() -> 
  [
    ?_assertEqual(dict:new(), fun_chain:last(dict:new())),
      
    ?_assertEqual(
      dict:from_list([{b,2}, {c,3}]),
      fun_chain:last(dict:new(),
        dict:store(a,1),
        dict:store(b,2),
        dict:erase(a),
        dict:store(c,3)
      )
    ),
    
    ?_assertEqual(
      dict:new(), 
      fun_chain:last(fun_chain:last(dict:new()))
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      fun_chain:last(dict:new(),
        (fun(Dict) ->
          dict:store(a,1,Dict)
        end)()
      )
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      fun_chain:last(dict:new(),
        (fun(Dict) ->
          fun_chain:last(Dict, dict:store(a,1))
        end)()
      )
    )
  ].