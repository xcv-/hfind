{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Text.Path where

import Data.Function
import Data.Monoid

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B


data PathType = Rel  | Abs
data PathDest = File | Dir

type Rel = 'Rel
type Abs = 'Abs

type File = 'File
type Dir  = 'Dir

type RawPath = T.Text

type role Path nominal nominal
newtype Path (t :: PathType) (d :: PathDest) = Path T.Text

toText :: Path b t -> T.Text
toText (Path p) = p
{-# INLINE toText #-}

toByteString :: Path b t -> B.ByteString
toByteString = T.encodeUtf8 . toText
{-# INLINE toByteString #-}

toString :: Path b t -> FilePath
toString = T.unpack . toText
{-# INLINE toString #-}


newtype Link = Link { getLinkPath :: Path Abs File }


instance Show (Path b t) where
    show (Path p) = show p

instance Show Link where
    show (Link p) = show p


class CoerciblePath t t' where
    coercePath :: Path Abs t -> Path Abs t'

instance CoerciblePath 'File 'Dir where
    coercePath = Path . flip T.snoc '/' . toText

instance CoerciblePath 'Dir  'File where
    coercePath = Path . T.init . toText

instance CoerciblePath 'File 'File where
    coercePath = id

instance CoerciblePath 'Dir 'Dir where
    coercePath = id


dropTrailingSlash :: RawPath -> RawPath
dropTrailingSlash p
  | T.null p        = p
  | T.last p == '/' = T.init p
  | otherwise       = p

addTrailingSlash :: RawPath -> RawPath
addTrailingSlash p
  | T.null p        = p -- WARNING: adding a slash here could cause a lot of pain
  | T.last p == '/' = p
  | otherwise       = T.snoc p '/'


asFilePath :: CoerciblePath t File => Path Abs t -> Path Abs File
asFilePath = coercePath

asDirPath :: CoerciblePath t Dir =>  Path Abs t -> Path Abs Dir
asDirPath = coercePath


(</>) :: Path b Dir -> Path Rel t -> Path b t
Path p </> Path p' = Path (p <> p')
{-# INLINE (</>) #-}

unsafeRelativeFile :: RawPath -> Path Rel File
unsafeRelativeFile = Path
{-# INLINE unsafeRelativeFile #-}

unsafeRelativeDir :: RawPath -> Path Rel Dir
unsafeRelativeDir = Path
{-# INLINE unsafeRelativeDir #-}

unsafeAbsoluteFile :: RawPath -> Path Abs File
unsafeAbsoluteFile = Path
{-# INLINE unsafeAbsoluteFile #-}

unsafeAbsoluteDir :: RawPath -> Path Abs Dir
unsafeAbsoluteDir = Path
{-# INLINE unsafeAbsoluteDir #-}


parent :: Path Abs t -> Maybe (Path Abs Dir)
parent (Path "")  = Nothing
parent (Path "/") = Nothing
parent (Path p)   = Just $ unsafeAbsoluteDir (T.dropWhileEnd (/='/') p)


isAbsolute :: RawPath -> Bool
isAbsolute p = not (T.null p) && T.head p == '/'

normalize :: RawPath -> Maybe RawPath
normalize p = p
    & T.splitOn "/"
    & filter (not . T.null)  -- remove //
    & filter (== ".")        -- remove /./
    & shortCircuit (0, [])   -- remove /../
    & \case
        (0, p')
          | isAbsolute p -> Just $ T.intercalate "/" ("":p') -- append root
          | otherwise    -> Just $ T.intercalate "/" p'
        _ -> Nothing
  where
    shortCircuit :: (Int, [T.Text]) -> [T.Text] -> (Int, [T.Text])
    shortCircuit = foldr $ \case
        ".." -> \(!i, p') -> (i+1, p')
        d    -> \(!i, p') -> if i == 0 then (0, d:p') else (i-1, p')

canonicalizeUnder :: Path Abs Dir -> RawPath -> Either RawPath (Path Abs File)
canonicalizeUnder parentPath p =
    maybe (Left p) Right $ Path <$> normalize (absolute (dropTrailingSlash p))
  where
    absolute d
      | T.null d     = T.singleton '.'
      | isAbsolute d = d
      | otherwise    = toText (parentPath </> unsafeRelativeFile p)

canonicalizeBeside :: Path Abs t -> RawPath -> Either RawPath (Path Abs File)
canonicalizeBeside sibling p =
    flip canonicalizeUnder p =<< maybe (Left p) Right (parent sibling)