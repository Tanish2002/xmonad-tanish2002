module Hooks.ManageHook
  ( manager,
  )
where

-- Imports --------------------------------------------------------------------

import Apps.Scratchpad
import XMonad
import XMonad.Hooks.BorderPerWindow (defineBorderWidth)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import qualified XMonad.StackSet as W (swapUp)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
-- ManageHook --------------------------------------------------------------------
manager :: ManageHook
manager =
  insertPosition Below Newer
    <+> namedScratchpadManageHook scratchpads
    <+> composeAll
      [ isDialog --> doF W.swapUp,
        className =? "MPlayer" --> doFloat,
        className =? "Gimp" --> doFloat,
        className =? "discord" --> doShift "\xfb6e",
        className =? "firefox" --> doShift "\xf269",
        className =? "postman" --> doShift "\xf044",
        className =? "code" --> doShift "\xe795",
        isFullscreen --> doFullFloat,
        (title =? "st" <&&> title =? "zsh"  --> defineBorderWidth 45),
        fullscreenManageHook
      ]
