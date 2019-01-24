module Brick.Widgets.FileTree
  (
  -- * Types
  FileTree
  , FileContext
  , flagged
  , val
  , path
  , name
  , kind

  -- Initialization
  , newFileTree

  -- * Interaction
  , moveUp
  , moveDown
  , pageUp
  , pageDown
  , moveToTop
  , moveToBottom
  , ascendDir
  , descendDir
  , toggleFlagged
  , toggleFlaggedVisible

  -- * Queries
  , getCurrentFilePath
  , getCurrentDir
  , getFlagged

  -- * Rendering
  , renderFileTree
  , renderFileTreeCustom
  , renderFileContext

  -- ** Attributes
  , flaggedItemAttr
  , titleAttr
  , dirAttr
  , fileAttr
  , errorAttr
  )
where

import           Brick.Widgets.FileTree.Internal.Types
import           Brick.Widgets.FileTree.Internal.Actions
import           Brick.Widgets.FileTree.Internal.Render
