# Shell commands as streams

<!--
This package provides a shell monad that supports shell like stateful
programming with global environment variables and execution of programs
from locations specified in the `PATH` environment variable.  You can
connect shell processes and Haskell functions seamlessly in the same
way as Unix pipes. You can find equivalents for most shell programming
constructs.

In addition to a native shell monad, this package also supports
interpreting scripts using external shells e.g. `sh`.
-->

A wrapper over `Streamly.System.Command` module (from `streamly-process`
package) to use `sh` as an interpreter for command strings. This package
just adds `sh -c` to a command to run it using shell. We can use
`Streamly.System.Command` directly instead. That we could use any shell
e.g. `bash -c`.

This is not part of the `streamly-process` package because this is POSIX
only and `streamly-process` is portable.
