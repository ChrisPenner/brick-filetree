{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Brick.Widgets.FileTree.Internal.Actions
  ( moveUp
  , moveDown
  , pageUp
  , pageDown
  , moveToTop
  , moveToBottom
  , ascendDir
  , descendDir
  , getCurrentFilePath
  , getCurrentDir
  , toggleFlagged
  , getFlagged
  , toggleFlaggedVisible
  ) where

import qualified Graphics.Vty.Input as V
import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Data.Foldable
import Control.Comonad.Cofree as CF
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Brick.Widgets.FileTree.Internal.Types
import Brick.Widgets.FileTree.Internal.Render
import Control.Monad.IO.Class
import Control.Comonad
import Data.Maybe

overCurrentList
  :: (List String SubTree -> EventM String (List String SubTree))
  -> FileTree
  -> EventM String FileTree
overCurrentList f fz@(FT { context = x :< lst }) = do
  newLst <- f lst
  return fz { context = x :< newLst }

pressKey :: V.Key -> (FileTree -> EventM String FileTree)
pressKey k = overCurrentList (handleListEvent (V.EvKey k []))

-- | Move the cursor down one item
moveDown :: FileTree -> EventM String FileTree
moveDown = pressKey V.KDown

-- | Move the cursor up one item
moveUp :: FileTree -> EventM String FileTree
moveUp = pressKey V.KUp

-- | Move the cursor down a page
pageDown :: FileTree -> EventM String FileTree
pageDown = pressKey V.KPageDown

-- | Move the cursor up a page
pageUp :: FileTree -> EventM String FileTree
pageUp = pressKey V.KPageDown

-- | Move the cursor the the top of the file list
moveToTop :: FileTree -> EventM String FileTree
moveToTop = pressKey V.KHome

-- | Move the cursor the the bottom of the file list
moveToBottom :: FileTree -> EventM String FileTree
moveToBottom = pressKey V.KEnd

-- | Move the cursor up a directory in the file tree
ascendDir :: FileTree -> EventM String FileTree
ascendDir (FT { parents = Seq.Empty, context = tree@((extract -> path -> p)), selection, ..})
  = do
    fz <- liftIO $ buildParent p tree
    return $ fz { selection = selection }
ascendDir (FT { parents = (ps Seq.:|> (f :< pList)), context, ..}) = do
  invalidateCacheEntry (cacheKey f)
  return
    $ FT {parents = ps, context = (f :< listModify (const context) pList), ..}

-- | If the cursor is on a directory then descend the cursor into that dir
-- If the cursor is on a file nothing happens
descendDir :: FileTree -> EventM String FileTree
descendDir fz@(FT { parents, context = (f :< children), ..}) = do
  invalidateCacheEntry (cacheKey f)
  return $ case listSelectedElement children of
    Nothing -> fz
    Just (_, nextChildren@(FC { kind = Dir } :< _)) -> FT
      { parents = (parents Seq.|> (f :< children))
      , context = nextChildren
      , ..
      }
    Just _ -> fz

-- | Get the absolute path of the object (dir or file) under the cursor 
getCurrentFilePath :: FileTree -> Maybe FilePath
getCurrentFilePath (FT { context = unwrap -> children }) =
  case listSelectedElement children of
    Nothing                            -> Nothing
    Just (_, FC { kind = Error } :< _) -> Nothing
    Just (_, fc :< _                 ) -> Just (path fc)

-- | Get the absolute path of the directory where the cursor currently is.
getCurrentDir :: FileTree -> FilePath
getCurrentDir (FT { context = extract -> path -> p }) = p

-- | Flag or unflag the current file or dir
toggleFlagged :: FileTree -> EventM String FileTree
toggleFlagged fz@(FT { context = (fc :< lst), selection, ..}) = do
  invalidateCacheEntry selectionCacheKey
  return . fromMaybe fz $ do
    ((selectedContext@FC { selected = isSelected, path }) :< rest) <- snd
      <$> listSelectedElement lst
    let newSelection = if isSelected
          then S.delete path selection
          else S.insert path selection
    let newList = listModify
          (const (selectedContext { selected = not isSelected } :< rest))
          lst
    return $ FT {context = (fc :< newList), selection = newSelection, ..}

-- | Get all flagged file paths. All paths are absolute
getFlagged :: FileTree -> [FilePath]
getFlagged = toList . selection

-- | Hide/Show a list of all flagged files
toggleFlaggedVisible :: FileTree -> FileTree
toggleFlaggedVisible fz@(FT { config }) =
  fz { config = config { showSelection = not $ showSelection config } }
