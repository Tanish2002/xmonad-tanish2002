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
manager = composeAll
    [ className =? "MPlayer"        --> doFloat
    -- , className =? "mpv"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "discord"        --> doShift "\xfb6e"
    , className =? "Firefox"        --> doShift "\xf269"
    , className =? "postman"        --> doShift "\xf044"
    , className =? "code"           --> doShift "\xe795"
    , isFullscreen                  --> doFullFloat
    , isDialog                      --> doF W.swapUp
    , fullscreenManageHook
    , insertPosition Below Newer]
    <+> manageScratchpad
