module Hooks.ManageHook
  ( manager,
  )
where

-- Imports --------------------------------------------------------------------

import Apps.Scratchpad
import Data.Ratio ((%))
import XMonad
import XMonad.Hooks.BorderPerWindow (defineBorderWidth)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import qualified XMonad.StackSet as W
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
        className =? "firefox" <&&> resource =? "Toolkit" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
        className =? "postman" --> doShift "\xf044",
        className =? "code" --> doShift "\xe795",
        isFullscreen --> doFullFloat,
        (title =? "st" <&&> title =? "zsh"  --> defineBorderWidth 45),
        fullscreenManageHook
      ]
