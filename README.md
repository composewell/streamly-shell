# Shell programming using streams

This package provides a shell monad that supports shell like stateful
programming with global environment variables and execution of programs
from locations specified in the `PATH` environment variable.  You can
connect shell processes and Haskell functions seamlessly in the same
way as Unix pipes. You can find equivalents for most shell programming
constructs.

In addition to a native shell monad, this package also supports
interpreting scripts using external shells e.g. `sh`.
