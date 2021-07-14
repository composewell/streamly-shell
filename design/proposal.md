----------------------
title: Shell Facilities
----------------------

`Streaming System Processes` covers shell facilities related to process
management. This task is to implement the remaining shell like functionality
for Streamly so that shell scripting can be used conveniently with existing
knowledge of shell and without learning much new stuff.

## Scope of the Project

_Shell built-ins_: Consider implementing the shell built-in commands wherever
it makes sense or identify and document the existing Haskell equivalents.

_Expansions_:  We can consider implementing tilde expansion, pathname
expansion, command substitution, process substitution etc.

_Environment_: The shell environment variables can be emulated by the
reader/state monad built into Streamly. We can create (`declare`) variables
using the `type` of the variable as a key and store it in the state monad in a
map. We can use something like `set` and `get` to modify and retrieve the
values of variables.

We can also use a combinator to create subshell like environments which will
inherit the parent environment but any changes to the environment in the
subshell will not reflect in the parent environment.

Also, consider if there are any shell built-in environment variables
that might be useful to support automatically.

_Monitoring_: `time` combinator like the shell built-in time command to measure
the timings of a pipeline.

## References

* See the [`bash` man page](https://linux.die.net/man/1/bash)
* http://hackage.haskell.org/package/turtle
* https://hackage.haskell.org/package/shelly
* https://hackage.haskell.org/package/shell-conduit

## Prerequisites

Working knowledge of the Haskell programming language is needed, advanced
knowledge is not required. If you have a good knowledge of basics and can work
with Haskell lists that should be enough.

## Difficulty Level

Intermediate.

## Mentors

* Harendra Kumar
* Pranay Sashank
