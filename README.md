# brick-filetree

[![Hackage](https://img.shields.io/hackage/v/brick-filetree.svg)](https://hackage.haskell.org/package/brick-filetree)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/brick-filetree/badge/lts)](http://stackage.org/lts/package/brick-filetree)
[![Stackageetree
Nightly](http://stackage.org/package/brick-fil/badge/nightly)](http://stackage.org/nightly/package/brick-filetree)
[![Build status](https://secure.travis-ci.org/ChrisPenner/brick-filetree.svg)](https://travis-ci.org/ChrisPenner/brick-filetree)

---

A brick widget for exploring file trees.

You can dig through my unfinished cli file browser
[Delve](https://github.com/ChrisPenner/delve) for a few examples if you like.

Note that this works relatively well, but I give no guarantees about future
maintenance or feature requests.

Allows selecting multiple files/directories from anywhere in your filesystem
which can be viewed from your brick app and used for whatever you like.

Note, this uses lazy IO to build the file tree, which will be run iteratively
as you use the UI to explore it; this allows better performance, but all the
usual lazyIO caveats apply; shouldn't cause you too much trouble though.

## Usage:

-   Create a FileTree using `newFileTree`
-   Store this FileTree in your Brick State somewhere
-   Render the FileTree using `renderFileTree`
-   Wire up handlers for brick events to trigger FileTree actions, e.g.
    `moveUp`, `toggleFlagged`, etc.
-   At some point you'll want to get the selected object with
    `getCurrentFilePath` or get all flagged items with `getFlagged`.

See [Hackage](https://hackage.haskell.org/package/brick-filetree) for docs.

![screencast](./screencast.gif)
