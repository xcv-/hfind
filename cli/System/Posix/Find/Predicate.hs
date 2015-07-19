{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Predicate
  ( DirPredicate(..)
  , EntryPredicate(..)
  , parsePrunePredicate
  , parseFilterPredicate
  ) where

import Control.Monad.Trans.Except

import qualified Data.Text as T

import System.Posix.Find


-- newtype FilePredicate = FilePredicate
--    { evalFilePredicate :: FSNode 'File 'Resolved -> Bool }

newtype DirPredicate = DirPredicate
    { evalDirPredicate :: FSNode 'Dir 'Resolved -> Bool }

newtype NodePredicate = NodePredicate
    { evalNodePredicate :: forall t. FSNode t 'Resolved -> Bool }

newtype EntryPredicate = EntryPredicate
    { evalEntryPredicate :: NodeListEntry 'Resolved -> Bool }


parsePrunePredicate :: String -> ExceptT String IO DirPredicate
parsePrunePredicate _ = return $ DirPredicate (const False)

parseFilterPredicate :: String -> ExceptT String IO EntryPredicate
parseFilterPredicate _ = return $ EntryPredicate (const True)
