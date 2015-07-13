module System.Posix.Find.Main (main) where

import System.Posix.Text.Path
import System.Posix.Find.Types
import System.Posix.Find.Combinators
import System.Posix.Find.Ls

main :: IO ()
main = do

    return ()

    --ls <- return . bimap nodePath nodePath . filterFilesN missing <-< yield (followLinks ls)

    --P.fold (\() _ -> ()) () id (sourceLs ls)
{-
    ls <- lsR =<< parseAbsFile =<< getHomeDirectory

    let missing (Missing _) = return True
        missing _           = return False

    let hidden d = return $ head (toFilePath $ dirname d) == '.'

    runEffect $ do
        yield (followLinks ls)
          >-> filterFilesN missing
          >-> P.map (bimap nodePath nodePath)
          >-> pruneDirs hidden
          >-> enumerateLs
          >-> P.print
          -}
