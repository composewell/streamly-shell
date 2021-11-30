# Haskell streamly-shell vs bash
XXX toBytes would write stderr to stderr itself just like unix pipes do.

toBytes_ would discard stderr.

toBytes' would produce a stream with both err/out.

## Exec with a spec

* Use process specs to control the behavior of the process

processBytesWith spec path

## Running shell commands

sh toBytes_ cmd
sh toChunks_ cmd
sh toUtf8_ cmd
sh toLines_ cmd -- Utf8 lines
sh toWords_ cmd -- Utf8 words

## Shell like facilities

### Running commands under shell env

* A global environment to control the exec behavior
* Use "Path" global env var to find the executable
* Use "Args" to hold the program arguments

cmd/run toBytes_ name -- cmd finds the path and supplies it to toBytes
We can use with/via/to like Sh/bash modules to keep it consistent.

* Sequential execution of effects: "type Shell = SerialT (ReaderT Env IO)" monad
* Global variables: Reader Monad (ShellT or Shell) with a variable Map, Static type => value
* StateT vs ReaderT with mutable vars? StateT may be better to reason about the
  mutations. We can guarantee that the mutations won't interfere, we can create
  copies.

### Subshell and inheritance

* local of StateT
* export

### Pipes

* & to compose streams like | in shell
* pipefail: exception propagation
* set -e: exceptions by default in streamly
* ! to disable pipefail?

### Chaining commands

* && => >>, do block
* || => <|> (Alternative instance of IO)

### Equivalents

* source => import
* $0: ?
* $@, $*
* $1: getArgs !! 0
* variable substitution ($var): haskell variables
* String interpolation ("x$var"): concat [], [str| |], ++, need better
* command substitution ($(cmd ..)): Stream.toList, Stream.toUtf8?, Stream.toArray?, Array.fromStream
* Functions: haskell functions
* for, while => map
* eval => can we generate Haskell code dynamically and execute it?
* conditonals => haskell conditionals
* case => haskell case

### Redirections

* putBytes, appendBytes
* Either streams (lefts, rights, both)

### Globbing

* in case
* in [[

Use parser combinators?

### Environment

* args
* environment (procenv/sysenv)
* dir stack
* path cache

We can use a (StateT Env m) and an opaque Env type with operations on it and no
constructors exported.

Alternatively we could use a custom newtype monad and custom operations in the
monad.

### Shell builtins (operating on the state of the shell)

* arg1/arg2/arg3 ...
* shift
* pushd
* popd
* hash => cached path purge

Global variable access:
* setvar/set
* getvar/get

Usually we should only need setenv/getenv and not any other global variables.
We should be able to do with haskell vars only.

Some operations do not depend on the state of the shell (e.g. getcwd) but can
be optimized if the values are cached in the state of the shell. Avoiding the
overhead of a system call.

* cd
* pwd
* getenv
* setenv (export)

### Commands

Coreutils package.

* getcwd (cd)
* echo
* cat
* pwd
* setenv
* getenv
* test
* mkdir
* mv
* cp
* dirname
* exit

* grep
* awk
* sed
