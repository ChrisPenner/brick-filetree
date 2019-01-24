{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.FileTree.Internal.Render
  ( selectedItemAttr
  , titleAttr
  , dirAttr
  , fileAttr
  , errorAttr
  , cacheKey
  , renderHeader
  , renderFileTree
  , selectionCacheKey
  , renderSelection
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

selectedItemAttr, titleAttr, dirAttr, fileAttr, errorAttr :: AttrName
selectedItemAttr = "selectedItemAttr"
titleAttr = "titleAttr"
dirAttr = "dirAttr"
fileAttr = "fileAttr"
errorAttr = "errorAttr"

cacheKey :: FileContext -> String
cacheKey = path

renderHeader :: SubTree -> Widget String
renderHeader ((path -> p) :< _) = withAttr titleAttr (str p) <=> hBorder

renderFileTree :: FileTree -> Widget String
renderFileTree fz@(FZ { parents, context, config }) =
  (   renderHeader context
  <=> (renderParents parents <+> renderNode context <+> previewW)
  <=> selectionW
  )
 where
  selectionW = if showSelection config then renderSelection fz else emptyWidget
  previewW   = if previewDir config then renderPreview context else emptyWidget

renderPreview :: SubTree -> Widget String
renderPreview (_ :< lst) = do
  case listSelectedElement lst of
    Just (_, node@(FC { kind = Dir } :< _)) -> vBorder <+> renderNode node
    _ -> emptyWidget

selectionCacheKey :: String
selectionCacheKey = "delve!selection"

renderSelection :: FileTree -> Widget String
renderSelection (FZ { selection })
  | null selection
  = emptyWidget
  | otherwise
  = let selectionsW =
          cached selectionCacheKey
            . vBox
            . fmap (withAttr selectedItemAttr . str)
            . toList
            $ selection
    in  hBorder <=> withAttr titleAttr (str "Selected") <=> selectionsW

renderParents :: S.Seq SubTree -> Widget String
renderParents S.Empty                    = emptyWidget
renderParents parents@(_ S.:|> (p :< _)) = cached
  (cacheKey p)
  (hBox . toList $ (renderParent <$> S.drop ind parents))
 where
  len = S.length parents
  ind = max 0 (len - 2)

renderNode :: SubTree -> Widget String
renderNode (_ :< ls) = renderList
  (\b -> bool id (forceAttr listSelectedAttr) b . renderFileContext . extract)
  True
  ls

renderParent :: SubTree -> Widget String
renderParent = (<+> vBorder) . hLimit 20 . renderNode

renderFileContext :: FileContext -> Widget String
renderFileContext (FC { kind = File, name, selected }) =
  let (attr', modStr) =
        if selected then (selectedItemAttr, "* ") else (fileAttr, "")
  in  withAttr attr' . str $ modStr <> name
renderFileContext (FC { kind = Error, name, path }) =
  withAttr errorAttr . str $ "! " <> path <> ": " <> name
renderFileContext (FC { kind = Dir, name, selected }) =
  let (attr', modStr) =
        if selected then (selectedItemAttr, "* ") else (dirAttr, "")
  in  withAttr attr' . str $ modStr <> name <> "/"
