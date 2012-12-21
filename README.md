# fun_chain

## Introduction

This simple parse_transform simplifies function chaining in Erlang.

Normally in Erlang when one needs to chain dict operations, one can write it as:

    D = dict:new(),
    D1 = dict:store(a, 1, D),
    D2 = dict:erase(a, D)

or as:

    dict:erase(a, dict:store(a, 1, dict:new()))
    
The fun_chain parse_transforms enables simpler syntax:

    fun_chain(dict:new(),
      dict:store(a, 1),
      dict:erase(a)
    )
    
The first argument of a fun_chain is an expression. All other arguments are function calls. The result of a previous call is automatically included as the last argument in the next function call. This eliminates the need for temp variables or staircasing.

The dict here serves only as an example. The fun_chain can be used for any set of function calls where the value of the last function is fed as the last argument to the next function call.


## Usage

Erlang R15 or higher required.
Just rebar compile and make sure the beams are in the load path.
Add following to your .erl:

    -compile({parse_transform, fun_chain}).

See tests for an example.