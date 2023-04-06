-- Imports --------------------------------------------------------------------

-- personal imports (./src/*)

import Bind.KeyBoard
import Bind.Mouse
import Config.Options
import Container.Layout
import Hooks.EventHook
import Hooks.LogHook
import Hooks.ManageHook
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Run (safeSpawn)

-- @TODO Make Config for Theming------------------------------------------------------------------------------------------------------
myNormalBorderColor :: [Char]
myNormalBorderColor = "#dddddd"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ff0000"

-- Main Function-------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  -- pipes
  safeSpawn
    "mkfifo"
    [ "/tmp/xmonad-layout-name"
    ]

  xmonad
    . addEwmhWorkspaceSort (pure scratchPadFilter)
    . ewmhFullscreen
    . ewmh
    $ fullscreenSupport $
      docks
        defaults

defaults =
  def
    { -- simple stuff
      terminal = term options,
      focusFollowsMouse = ffm options,
      clickJustFocuses = clickfocus options,
      borderWidth = border options,
      modMask = modkey options,
      workspaces = spaces options,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      mouseBindings = mouse,
      -- hooks, layouts
      layoutHook = layout,
      manageHook = manager,
      handleEventHook = events,
      logHook = logger,
      startupHook = autoload options >> addEWMHFullscreen
    }
    `removeKeysP` [ "M-q" --, "M-something"
                  ]
      `additionalKeysP` keyboard
