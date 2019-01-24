{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brick.Widgets.FileTree.Internal.Types
  ( FileKind(..)
  , FileContext(..)
  , Config(..)
  , FileTree(..)
  , SubTree
  , buildParent
  , newFileTree
  , defaultConfig
  )
where

import           Brick.Widgets.List
import qualified Data.Vector                   as V
import           Control.Comonad.Cofree        as CF
import qualified System.Directory.Tree         as FT
import qualified Data.Sequence                 as S
import           System.FilePath.Posix
import           System.Directory
import qualified Data.Set                      as S
import Data.List
import System.IO.Unsafe

data FileKind = Dir | File | Error
  deriving (Eq, Ord, Show)

type ValueLoader a = FileKind -> FilePath -> IO a

data FileContext a =
  FC
    { flagged :: Bool
    , path :: FilePath
    , name :: String
    , kind :: FileKind
    , val :: a
    }

data Config =
  Config
    { showSelection :: Bool
    , previewDir :: Bool
    }

defaultConfig :: Config
defaultConfig = Config {showSelection = True, previewDir = False}

type SubTree a = Cofree (GenericList String V.Vector) (FileContext a)

-- | Represents all the state required to interact with or display a filetree
data FileTree a = FT
  { parents :: S.Seq (SubTree a)
  , selection :: S.Set FilePath
  , context :: SubTree a
  , config :: Config
  , valLoader :: ValueLoader a
  }

buildParent :: FilePath -> ValueLoader a -> SubTree a -> IO (FileTree a)
buildParent p valLoader child = do
  FT { context = (c :< ls), ..} <- newFileTree valLoader (takeDirectory p)
  let newChildren = fmap (replace p child) ls
  return $ FT {context = c :< newChildren, ..}
 where
  replace pth fc@((path -> pth') :< _) new | pth == pth' = new
                                           | otherwise   = fc

-- | Create a new 'FileTree' situated at the given 'FilePath'
newFileTree :: ValueLoader a -> FilePath -> IO (FileTree a)
newFileTree valLoader currentDir = do
  absRoot        <- makeAbsolute (normalise currentDir)
  (_ FT.:/ tree) <- FT.readDirectoryWithL (interleavedValLoader File) absRoot
  convert interleavedValLoader (takeDirectory absRoot) tree
  where interleavedValLoader fk fp = unsafeInterleaveIO $ valLoader fk fp

convert
  :: forall a . ValueLoader a -> FilePath -> FT.DirTree a -> IO (FileTree a)
convert valLoader root tree = do
  subTree <- go (normalise root) tree
  pure $ FT
    { parents   = []
    , selection = mempty
    , config    = defaultConfig
    , context   = subTree
    , valLoader = valLoader
    }
 where
  go :: FilePath -> FT.DirTree a -> IO (SubTree a)
  go root' (FT.Failed { FT.name, FT.err }) = do
    val <- valLoader Error name
    pure
      $  FC
           { name    = show err
           , path    = normalise (root' </> name)
           , flagged = False
           , kind    = Error
           , val     = val
           }
      :< list name mempty 1
  go root' (FT.File { FT.name, FT.file }) =
    pure
      $  FC
           { name    = name
           , path    = normalise (root' </> name)
           , flagged = False
           , kind    = File
           , val     = file
           }
      :< list name mempty 1
  go root' (FT.Dir path contents) = do
    val      <- valLoader Dir path
    children <- traverse (go (root' </> path)) contents
    pure
      $  FC
           { name    = path <> "/"
           , path    = normalise (root' </> path)
           , kind    = Dir
           , flagged = False
           , val     = val
           }
      :< list path (V.fromList . sortOn byFileType $ children) 1

byFileType :: SubTree a -> (FileKind, String)
byFileType (FC { kind, name } :< _) = (kind, name)
