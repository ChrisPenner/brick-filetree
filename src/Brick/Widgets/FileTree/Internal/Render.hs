{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.FileTree.Internal.Render
  ( flaggedItemAttr
  , titleAttr
  , dirAttr
  , fileAttr
  , errorAttr
  , cacheKey
  , renderHeader
  , renderFileTree
  , renderFileTreeCustom
  , selectionCacheKey
  , renderFileContext
  )
where

import           Brick.Widgets.FileTree.Internal.Types

import           Data.Foldable
import           Brick.Widgets.Core
import           Brick.Widgets.Border
import           Brick.Types
import           Brick.AttrMap
import           Brick.Widgets.List
import           Control.Comonad.Cofree        as CF
import           Control.Comonad
import           Data.Bool
import qualified Data.Sequence                 as S

type CustomFCRender a = FileContext a -> Widget String

-- | Flagged items are rendered with this attr
flaggedItemAttr :: AttrName
flaggedItemAttr = "flaggedItemAttr"
-- | UI Titles have this attr
titleAttr :: AttrName
titleAttr = "titleAttr"
-- | Directories in the list have this attr
dirAttr :: AttrName
dirAttr = "dirAttr"
-- | Files in the list have this attr
fileAttr :: AttrName
fileAttr = "fileAttr"
-- | Errors have this attr
errorAttr :: AttrName
errorAttr = "errorAttr"

cacheKey :: FileContext a -> String
cacheKey = path

renderHeader :: SubTree a -> Widget String
renderHeader ((path -> p) :< _) =
  withAttr titleAttr (str $ p <> "/") <=> hBorder

renderFileTreeCustom :: CustomFCRender a -> FileTree a -> Widget String
renderFileTreeCustom customFCRender fz@(FT { parents, context, config }) =
  (   renderHeader context
  <=> (   renderParents customFCRender parents
      <+> renderNode customFCRender True context
      <+> previewW
      )
  <=> selectionW
  )
 where
  selectionW = if showSelection config then renderSelection fz else emptyWidget
  previewW   = if previewDir config
    then renderPreview customFCRender context
    else emptyWidget

renderFileTree :: FileTree a -> Widget String
renderFileTree = renderFileTreeCustom renderFileContext

renderPreview :: CustomFCRender a -> SubTree a -> Widget String
renderPreview customFCRender (_ :< lst) = do
  case listSelectedElement lst of
    Just (_, node@(FC { kind = Dir } :< _)) ->
      vBorder <+> renderNode customFCRender False node
    _ -> emptyWidget

selectionCacheKey :: String
selectionCacheKey = "delve!selection"

renderSelection :: FileTree a -> Widget String
renderSelection (FT { selection })
  | null selection
  = emptyWidget
  | otherwise
  = let selectionsW =
          cached selectionCacheKey
            . vBox
            . fmap (withAttr flaggedItemAttr . str)
            . toList
            $ selection
    in  hBorder <=> withAttr titleAttr (str "flagged") <=> selectionsW

renderParents :: CustomFCRender a -> S.Seq (SubTree a) -> Widget String
renderParents _              S.Empty                    = emptyWidget
renderParents customFCRender parents@(_ S.:|> (p :< _)) = cached
  (cacheKey p)
  (hBox . toList $ (renderParent customFCRender <$> S.drop ind parents))
 where
  len = S.length parents
  ind = max 0 (len - 2)

renderNode :: CustomFCRender a -> Bool -> SubTree a -> Widget String
renderNode customFCRender focused (_ :< ls) = renderList
  (\b -> bool id (forceAttr listSelectedAttr) b . customFCRender . extract)
  focused
  ls

renderParent :: CustomFCRender a -> SubTree a -> Widget String
renderParent customFCRender =
  (<+> vBorder) . hLimit 20 . renderNode customFCRender False

renderFileContext :: FileContext a -> Widget String
renderFileContext (FC { kind = File, name, flagged }) =
  let (attr', modStr) =
        if flagged then (flaggedItemAttr, "* ") else (fileAttr, "")
  in  withAttr attr' . str $ modStr <> name
renderFileContext (FC { kind = Error, name, path }) =
  withAttr errorAttr . str $ "! " <> path <> ": " <> name
renderFileContext (FC { kind = Dir, name, flagged }) =
  let (attr', modStr) =
        if flagged then (flaggedItemAttr, "* ") else (dirAttr, "")
  in  withAttr attr' . str $ modStr <> name
