-module(fun_chain_tests).
-compile({parse_transform, fun_chain}).

-include_lib("eunit/include/eunit.hrl").

simple_test_() -> 
  [
    ?_assertEqual(dict:new(), fun_chain(dict:new())),
      
    ?_assertEqual(
      dict:from_list([{b,2}, {c,3}]),
      fun_chain(dict:new(),
        dict:store(a,1),
        dict:store(b,2),
        dict:erase(a),
        dict:store(c,3)
      )
    ),
    
    ?_assertEqual(
      dict:new(), 
      fun_chain(fun_chain(dict:new()))
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      fun_chain(dict:new(),
        (fun(Dict) ->
          dict:store(a,1,Dict)
        end)()
      )
    ),
    
    ?_assertEqual(
      dict:from_list([{a,1}]), 
      fun_chain(dict:new(),
        (fun(Dict) ->
          fun_chain(Dict, dict:store(a,1))
        end)()
      )
    )
  ].