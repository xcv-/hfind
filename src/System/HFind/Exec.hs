module System.HFind.Exec
  ( forkExecWithExitCode
  , forkExecOrThrow
  , forkExecDirWithExitCode
  , forkExecDirOrThrow
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Except   (throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified System.Process as Proc
import System.IO (hFlush, stdout, stderr)
import System.Exit (ExitCode(..))

import System.HFind.Path (Path, Abs, Dir)
import qualified System.HFind.Path as Path

import System.HFind.Expr.Eval (Eval)
import System.HFind.Expr.Error (RuntimeError(..))


procTextArgs :: Text -> [Text] -> Proc.CreateProcess
procTextArgs tcmd targs =
    let cmd  = T.unpack tcmd
        args = map T.unpack targs
    in  Proc.proc cmd args

forkWithExitCode :: MonadIO m => Proc.CreateProcess -> m ExitCode
forkWithExitCode proc = liftIO $ do
    hFlush stdout
    hFlush stderr
    ec <- Proc.withCreateProcess proc $ \_ _ _ ->
              Proc.waitForProcess
    hFlush stdout
    hFlush stderr
    return ec

forkExecWithExitCode :: MonadIO m => Text -> [Text] -> m ExitCode
forkExecWithExitCode tcmd targs =
    forkWithExitCode (procTextArgs tcmd targs)

forkExecDirWithExitCode :: MonadIO m => Path Abs Dir -> Text -> [Text] -> m ExitCode
forkExecDirWithExitCode path tcmd targs =
    forkWithExitCode (procTextArgs tcmd targs) {
        Proc.cwd = Just (T.unpack (Path.toText path))
    }

checkExitCode :: Text -> [Text] -> ExitCode -> Eval ()
checkExitCode _   _    ExitSuccess      = return ()
checkExitCode cmd args (ExitFailure ec) = throwError (NonZeroExitCode cmd args ec)

forkExecOrThrow :: Text -> [Text] -> Eval ()
forkExecOrThrow tcmd targs =
    checkExitCode tcmd targs =<< forkExecWithExitCode tcmd targs

forkExecDirOrThrow :: Path Abs Dir -> Text -> [Text] -> Eval ()
forkExecDirOrThrow path tcmd targs =
    checkExitCode tcmd targs =<< forkExecDirWithExitCode path tcmd targs
