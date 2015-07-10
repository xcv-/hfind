module System.Posix.Find.Main (main) where

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
