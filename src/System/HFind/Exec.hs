module System.HFind.Exec
  ( ExitCodeException(..)
  , forkExecWithExitCode
  , forkExecOrThrow
  , forkExecDirWithExitCode
  , forkExecDirOrThrow
  ) where

import Control.Exception (Exception, throwIO)

import Data.Text (Text)
import qualified Data.Text as T

import qualified System.Process as Proc
import System.IO (hFlush, stdout, stderr)
import System.Exit (ExitCode(..))

import System.HFind.Path (Path, Abs, Dir)
import qualified System.HFind.Path as Path


data ExitCodeException = NonZeroExitCodeError Int
    deriving (Eq, Show)

instance Exception ExitCodeException

procTextArgs :: Text -> [Text] -> Proc.CreateProcess
procTextArgs tcmd targs =
    let cmd  = T.unpack tcmd
        args = map T.unpack targs
    in  Proc.proc cmd args

forkWithExitCode :: Proc.CreateProcess -> IO ExitCode
forkWithExitCode proc = do
    hFlush stdout
    hFlush stderr
    ec <- Proc.withCreateProcess proc $ \_ _ _ ->
              Proc.waitForProcess
    hFlush stdout
    hFlush stderr
    return ec

forkExecWithExitCode :: Text -> [Text] -> IO ExitCode
forkExecWithExitCode tcmd targs =
    forkWithExitCode (procTextArgs tcmd targs)

forkExecDirWithExitCode :: Path Abs Dir -> Text -> [Text] -> IO ExitCode
forkExecDirWithExitCode path tcmd targs =
    forkWithExitCode (procTextArgs tcmd targs) {
        Proc.cwd = Just (T.unpack (Path.toText path))
    }

checkExitCode :: ExitCode -> IO ()
checkExitCode ExitSuccess      = return ()
checkExitCode (ExitFailure ec) = throwIO (NonZeroExitCodeError ec)

forkExecOrThrow :: Text -> [Text] -> IO ()
forkExecOrThrow tcmd targs =
    checkExitCode =<< forkExecWithExitCode tcmd targs

forkExecDirOrThrow :: Path Abs Dir -> Text -> [Text] -> IO ()
forkExecDirOrThrow path tcmd targs =
    checkExitCode =<< forkExecDirWithExitCode path tcmd targs
