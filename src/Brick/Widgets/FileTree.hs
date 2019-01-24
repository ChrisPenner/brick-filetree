module Brick.Widgets.FileTree
  (
  -- * Types
  FileTree

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
  , renderSelection

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
