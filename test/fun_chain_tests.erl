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
    )
  ].