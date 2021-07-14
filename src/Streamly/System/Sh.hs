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
-- >>> from = Sh.with Process.toBytes_
-- >>> via = Sh.via Process.processBytes_
-- >>> :{
--   from "echo hello"
-- & via "tr [a-z] [A-Z]"
-- & Stream.fold Stdio.write
-- :}
-- HELLO
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Streamly.System.Sh
    (
      with
    , via
    )
where

import Data.Kind (Type)

-- The APIs are named/designed such that we can replace the sh module with
-- another module for bash or any other shells without requiring API name
-- changes.

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.System.Process as Process

-- | A modifier for stream generation process APIs to generate streams from
-- shell scripts with "/bin/sh" as the shell interpreter.
--
-- >>> with Process.toBytes_ "echo hello" & Stream.fold Stdio.write
-- hello
-- >>> with Process.toChunks_ "echo hello" & Stream.fold Stdio.writeChunks
-- hello
--
-- >>> with f cmd = f "/bin/sh" ["-c", cmd]
--
with :: forall (t :: (Type -> Type) -> Type -> Type) m a.
    (FilePath -> [String] -> t m a) -> String -> t m a
with f cmd = f "/bin/sh" ["-c", cmd]

-- | A modifier for stream transformation process APIs to transform streams
-- using shell scripts with "/bin/sh" as the shell interpreter.
--
-- >>> via Process.processBytes_ "tr [a-z] [A-Z] hello" & Stream.fold Stdio.write
-- HELLO
-- >>> via Process.processChunks_ "tr [a-z] [A-Z} hello" & Stream.fold Stdio.writeChunks
-- HELLO
--
-- >>> via f cmd = f "/bin/sh" ["-c", cmd]
--
via :: forall (t :: (Type -> Type) -> Type -> Type) m a b.
    (FilePath -> [String] -> t m a -> t m b) -> String -> t m a -> t m b
via f cmd = f "/bin/sh" ["-c", cmd]
