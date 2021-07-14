-- |
-- Module      : Streamly.System.Sh
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Use shell scripts in your Haskell programs, interfacing via standard input
-- and output.  The functions in this module are just convenience wrappers over
-- "Streamly.System.Process" to run shell commands using "/bin/sh".
--
-- >>> src = srcWith Process.toBytes
-- >>> pipe = pipeWith Process.processBytes
-- >>> :{
--   src "echo hello"
-- & pipe "tr [a-z] [A-Z]"
-- & Stream.fold Stdio.write
-- :}
-- HELLO
--

module Streamly.System.Sh
    (
      srcWith
    , pipeWith
    )
where

import Streamly.Prelude (SerialT)

-- The APIs are named/designed such that we can replace the sh module with
-- another module for bash or any other shells without requiring API name
-- changes.

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -package streamly
-- >>> :set -package streamly-process
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.System.Process as Process
-- >>> import qualified Streamly.Unicode.Stream as Unicode

-- | A modifier for stream generation process APIs to generate streams from
-- shell scripts with "/bin/sh" as the shell interpreter.
--
-- >>> srcWith Process.toBytes "echo hello" & Stream.fold Stdio.write
-- hello
-- >>> srcWith Process.toChunks "echo hello" & Stream.fold Stdio.writeChunks
-- hello
--
-- >>> srcWith f cmd = f "/bin/sh" ["-c", cmd]
--
srcWith :: (FilePath -> [String] -> SerialT m a) -> String -> SerialT m a
srcWith f cmd = f "/bin/sh" ["-c", cmd]

-- | A modifier for stream transformation process APIs to transform streams
-- using shell scripts srcWith "/bin/sh" as the shell interpreter.
--
-- >>> :{
--    Stream.fromList "hello"
--  & Unicode.encodeLatin1
--  & pipeWith Process.processBytes "tr [a-z] [A-Z]"
--  & Stream.fold Stdio.write
--  :}
--HELLO
--
-- >>> :{
--    srcWith Process.toChunks "echo hello"
--  & pipeWith Process.processChunks "tr [a-z] [A-Z]"
--  & Stream.fold Stdio.writeChunks
--  :}
--HELLO
--
-- >>> pipeWith f cmd = f "/bin/sh" ["-c", cmd]
--
pipeWith ::
       (FilePath -> [String] -> SerialT m a -> SerialT m b)
    -> String
    -> SerialT m a
    -> SerialT m b
pipeWith f cmd = f "/bin/sh" ["-c", cmd]
