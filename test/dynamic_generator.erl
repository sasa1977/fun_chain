% This module dynamically creates, compiles and executes test code.
% This is used when testing parse_transform, so that the transform can be executed in the runtime
% during the test. By doing this, we can check code coverage of the transform. In addition,
% we can check that transform correctly generates errors.

-module(dynamic_generator).
-export([exec_dynamic/3, compile/4]).

-include_lib("eunit/include/eunit.hrl").

exec_dynamic(ModuleName, Head, Code) -> 
  compile(ModuleName, Head, Code, fun(CompilationResult) ->
    ?assertEqual({ok, ModuleName}, CompilationResult),
    code:purge(ModuleName),
    code:load_file(ModuleName),
    ModuleName:test()
  end).

compile(ModuleName, Head, Code, Fun) ->
  file:write_file(add_ext(ModuleName, "erl"), module_code(ModuleName, Head, Code)),
  try
    Fun(compile:file(add_ext(ModuleName, "erl"), [return_errors]))
  after
    file:delete(add_ext(ModuleName, "erl")),
    file:delete(add_ext(ModuleName, "beam"))
  end.

module_code(ModuleName, Head, Code) -> 
  Lines = [
    ["-module('", atom_to_list(ModuleName), "')."],
    "-export([test/0]).",
    Head,
    
    ["test() ->", Code, "."]
  ],
  [[Line, "\n"] || Line <- Lines].

add_ext(ModuleName, Extension) -> io_lib:format("~p.~s", [ModuleName, Extension]).