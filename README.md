# Introduction

This parse transform simplifies function chaining in Erlang.

Normally in Erlang when one needs to chain dict operations, one can write it as:

    D = dict:new(),
    D1 = dict:store(a, 1, D),
    D2 = dict:erase(a, D)

or as:

    dict:erase(a, dict:store(a, 1, dict:new()))
    
The fun_chain parse transforms enables simpler syntax:

    fun_chain:last(dict:new(),
      dict:store(a, 1),
      dict:erase(a)
    )
    
The first argument of a fun_chain:last is an expression. All other arguments are function calls. The result of a previous call is automatically included as the last argument in the next call. This eliminates the need for temp variables or staircasing.

The dict here serves only as an example. The fun_chain can be used for any set of function calls where the return value of the last call is fed as the last argument to the next call.


# Usage

Erlang R15 or higher required. Compile with rebar:

    rebar compile

or manually:

    mkdir -p ebin; erlc -o ebin/ src/*.erl

Once beams are compiled, make sure they are in the load path for erlc. Add following to your .erl files:

    -compile({parse_transform, fun_chain}).

See tests for an example.