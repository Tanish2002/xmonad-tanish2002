
-- Imports --------------------------------------------------------------------
import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.Fullscreen


-- personal imports (./src/*)
import Hooks.ManageHook
import Hooks.EventHook
import Hooks.LogHook
import Bind.KeyBoard
import Bind.Mouse
import Config.Options
import Container.Layout


-- @TODO Make Config for Theming------------------------------------------------------------------------------------------------------
myNormalBorderColor :: [Char]
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ff0000"

-- Main Function-------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = xmonad . ewmh $ fullscreenSupport $ docks defaults

defaults = def {
      -- simple stuff
        terminal           = term options,
        focusFollowsMouse  = ffm options,
        clickJustFocuses   = clickfocus options,
        borderWidth        = border options,
        modMask            = modkey options,
        workspaces         = spaces options,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        mouseBindings      = mouse,

      -- hooks, layouts
        layoutHook         = layout,
        manageHook         = manager,
        handleEventHook    = events,
        logHook            = logger,
        startupHook        = autoload options >> addEWMHFullscreen
    }`additionalKeysP` keyboard