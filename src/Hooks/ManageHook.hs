module Hooks.ManageHook
  ( manager
  )
where

-- Imports --------------------------------------------------------------------
import           XMonad

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import Apps.Scratchpad
import qualified XMonad.StackSet as W

-- ManageHook --------------------------------------------------------------------
manager :: ManageHook
manager = insertPosition Below Newer
    <+> manageScratchpad
    <+> composeAll
    [ isDialog                      --> doF W.swapUp
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "discord"        --> doShift "\xfb6e"
    , className =? "firefox"        --> doShift "\xf269"
    , className =? "postman"        --> doShift "\xf044"
    , className =? "code"           --> doShift "\xe795"
    , isFullscreen                  --> doFullFloat
    , fullscreenManageHook ]
