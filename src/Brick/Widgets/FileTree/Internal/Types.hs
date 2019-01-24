{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
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

data FileKind = Dir | File | Error
  deriving (Eq, Ord, Show)

data FileContext =
  FC
    { selected :: Bool
    , path :: FilePath
    , name :: String
    , kind :: FileKind
    }

data Config =
  Config
    { showSelection :: Bool
    , previewDir :: Bool
    }

defaultConfig :: Config
defaultConfig = Config {showSelection = True, previewDir = False}

type SubTree = Cofree (GenericList String V.Vector) FileContext

-- | Represents all the state required to interact with or display a filetree
data FileTree = FT
  { parents :: S.Seq SubTree
  , selection :: S.Set FilePath
  , context :: SubTree
  , config :: Config
  }

buildParent :: FilePath -> SubTree -> IO FileTree
buildParent p child = do
  FT { context = (c :< ls), ..} <- newFileTree (takeDirectory p)
  let newChildren = fmap (replace p child) ls
  return $ FT {context = c :< newChildren, ..}
 where
  replace pth fc@((path -> pth') :< _) new | pth == pth' = new
                                           | otherwise   = fc

-- | Create a new 'FileTree' situated at the given 'FilePath'
newFileTree :: FilePath -> IO FileTree
newFileTree currentDir = do
  absRoot        <- makeAbsolute (normalise currentDir)
  (_ FT.:/ tree) <- FT.buildL absRoot
  return $ convert (takeDirectory absRoot) tree

convert :: FilePath -> FT.DirTree FilePath -> FileTree
convert root tree =
  let subTree = go (normalise root) $ tree
  in  FT
        { parents   = []
        , selection = mempty
        , config    = defaultConfig
        , context   = subTree
        }
 where
  go :: FilePath -> FT.DirTree FilePath -> SubTree
  go root' (FT.Failed { FT.name, FT.err }) =
    FC
        { name     = show err
        , path     = normalise (root' </> name)
        , selected = False
        , kind     = Error
        }
      :< list name mempty 1
  go root' (FT.File { FT.name }) =
    FC
        { name     = name
        , path     = normalise (root' </> name)
        , selected = False
        , kind     = File
        }
      :< list name mempty 1
  go root' (FT.Dir path contents) =
    FC
        { name     = path <> "/"
        , path     = normalise (root' </> path)
        , kind     = Dir
        , selected = False
        }
      :< list
           path
           ( V.fromList
           . sortOn byFileType
           . fmap (go (root' </> path))
           $ contents
           )
           1

byFileType :: SubTree -> (FileKind, String)
byFileType (FC { kind, name } :< _) = (kind, name)
