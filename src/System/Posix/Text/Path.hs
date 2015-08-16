{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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

import qualified System.Directory as D

import Unsafe.Coerce


data PathMode = Rel  | Abs
data PathType = File | Dir

type Rel = 'Rel
type Abs = 'Abs

type File = 'File
type Dir  = 'Dir

type RawPath = T.Text

type role Path nominal nominal
newtype Path (t :: PathMode) (d :: PathType) = Path T.Text
    deriving (Eq, Ord)

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


class IsPathType (t :: PathType) where
    getPathType :: p t -> PathType

instance IsPathType 'File where
    getPathType _ = File

instance IsPathType 'Dir where
    getPathType _ = Dir


dropTrailingSlash :: RawPath -> RawPath
dropTrailingSlash p
  | p == "/"        = -- WARNING: removing the slash here could cause a lot of pain
                      error "'/' passed to dropTrailingSlash"
  | T.null p        = p
  | T.last p == '/' = T.init p
  | otherwise       = p

addTrailingSlash :: RawPath -> RawPath
addTrailingSlash p
  | T.null p        = -- WARNING: adding a slash here could cause a lot of pain
                      error "'' passed to addTrailingSlash"
  | T.last p == '/' = p
  | otherwise       = T.snoc p '/'


asFilePath :: Path Abs t -> Path Abs File
asFilePath (Path "")  = Path ""
asFilePath (Path "/") = Path ""
asFilePath (Path p)   = Path (dropTrailingSlash p)

asDirPath :: Path Abs t -> Path Abs Dir
asDirPath (Path "")  = Path "/"
asDirPath (Path "/") = Path "/"
asDirPath (Path p)   = Path (addTrailingSlash p)

coercePath :: forall t t'. IsPathType t' => Path Abs t -> Path Abs t'
coercePath p =
    case getPathType (undefined :: Path Abs t') of
        File -> unsafeCoerce (asFilePath p)
        Dir  -> unsafeCoerce (asDirPath p)


(</>) :: Path b Dir -> Path Rel t -> Path b t
Path p </> Path p' = Path (p <> p')
{-# INLINE (</>) #-}

unsafeRelFile :: RawPath -> Path Rel File
unsafeRelFile = Path

unsafeRelDir :: RawPath -> Path Rel Dir
unsafeRelDir = Path

unsafeAbsFile :: RawPath -> Path Abs File
unsafeAbsFile = Path

unsafeAbsDir :: RawPath -> Path Abs Dir
unsafeAbsDir = Path



unsnocPath :: Path Abs t -> Maybe (Path Abs Dir, T.Text)
unsnocPath (Path "")  = Nothing
unsnocPath (Path "/") = Nothing
unsnocPath (Path fp)  = Just (unsafeAbsDir dir, base)
  where
    (dir, base) = let noTrailing = T.dropWhileEnd (=='/') fp
                  in T.breakOnEnd "/" noTrailing

parent :: Path Abs t -> Maybe (Path Abs Dir)
parent = fmap fst . unsnocPath

filename :: Path Abs t -> Maybe T.Text
filename = fmap snd . unsnocPath


isAbsolute :: RawPath -> Bool
isAbsolute p = T.null p || T.head p == '/'

normalize :: RawPath -> Maybe RawPath
normalize p = p
    & T.splitOn "/"
    & filter (not . T.null)  -- remove //
    & filter (/= ".")        -- remove /./
    & shortCircuit (0, [])   -- remove /../
    & \case
        (0, p')
          | T.null p     -> Nothing
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
    maybe (Left p) Right $ Path <$> normalize (absolute p)
  where
    absolute d
      | isAbsolute d = d
      | otherwise    = toText (parentPath </> unsafeRelFile p)

canonicalizeBeside :: Path Abs t -> RawPath -> Either RawPath (Path Abs File)
canonicalizeBeside sibling p =
    flip canonicalizeUnder p =<< maybe (Left p) Right (parent sibling)

canonicalizeFromHere :: RawPath -> IO (Either RawPath (Path Abs File))
canonicalizeFromHere p = do
    here <- unsafeAbsDir . addTrailingSlash . T.pack <$> D.getCurrentDirectory
    return (canonicalizeUnder here p)
