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
  , toggleSelection
  , getFlagged
  , toggleSelectionVisible
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
overCurrentList f fz@(FZ { context = x :< lst }) = do
  newLst <- f lst
  return fz { context = x :< newLst }

pressKey :: V.Key -> (FileTree -> EventM String FileTree)
pressKey k = overCurrentList (handleListEvent (V.EvKey k []))

moveDown :: FileTree -> EventM String FileTree
moveDown = pressKey V.KDown

moveUp :: FileTree -> EventM String FileTree
moveUp = pressKey V.KUp

pageDown :: FileTree -> EventM String FileTree
pageDown = pressKey V.KPageDown

pageUp :: FileTree -> EventM String FileTree
pageUp = pressKey V.KPageDown

moveToTop :: FileTree -> EventM String FileTree
moveToTop = pressKey V.KHome

moveToBottom :: FileTree -> EventM String FileTree
moveToBottom = pressKey V.KEnd

ascendDir :: FileTree -> EventM String FileTree
ascendDir (FZ { parents = Seq.Empty, context = tree@((extract -> path -> p)), selection, ..})
  = do
    fz <- liftIO $ buildParent p tree
    return $ fz { selection = selection }
ascendDir (FZ { parents = (ps Seq.:|> (f :< pList)), context, ..}) = do
  invalidateCacheEntry (cacheKey f)
  return
    $ FZ {parents = ps, context = (f :< listModify (const context) pList), ..}

descendDir :: FileTree -> EventM String FileTree
descendDir fz@(FZ { parents, context = (f :< children), ..}) = do
  invalidateCacheEntry (cacheKey f)
  return $ case listSelectedElement children of
    Nothing -> fz
    Just (_, nextChildren@(FC { kind = Dir } :< _)) -> FZ
      { parents = (parents Seq.|> (f :< children))
      , context = nextChildren
      , ..
      }
    Just _ -> fz

getCurrentFilePath :: FileTree -> Maybe FilePath
getCurrentFilePath (FZ { context = unwrap -> children }) =
  case listSelectedElement children of
    Nothing                            -> Nothing
    Just (_, FC { kind = Error } :< _) -> Nothing
    Just (_, fc :< _                 ) -> Just (path fc)

getCurrentDir :: FileTree -> FilePath
getCurrentDir (FZ { context = extract -> path -> p }) = p

toggleSelection :: FileTree -> EventM String FileTree
toggleSelection fz@(FZ { context = (fc :< lst), selection, ..}) = do
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
    return $ FZ {context = (fc :< newList), selection = newSelection, ..}

getFlagged :: FileTree -> [FilePath]
getFlagged = toList . selection

toggleSelectionVisible :: FileTree -> FileTree
toggleSelectionVisible fz@(FZ { config }) =
  fz { config = config { showSelection = not $ showSelection config } }
