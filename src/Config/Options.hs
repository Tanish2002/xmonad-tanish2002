module Config.Options where

-- Imports --------------------------------------------------------------------

import Apps.Alias
import XMonad
import XMonad.Hooks.SetWMName
import qualified XMonad.Prompt as P
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

-- Types ------------------------------------------------------------------------
data Options = Options
  { term :: String,
    ffm :: Bool,
    clickfocus :: Bool,
    border :: Dimension,
    modkey :: KeyMask,
    spaces :: [String],
    autoload :: X ()
  }

-- Functions --------------------------------------------------------------------
options :: Options
options =
  Options
    { term = myterm,
      ffm = False,
      clickfocus = False,
      border = 0,
      modkey = mod4Mask,
      spaces = ["\xf269", "\xe795", "\xfb8a", "\xf044", "\xf675", "\xfb6e"],
      autoload =
        setWMName "XMonad"
          >> spawnOnce battery
          >> spawnOnce bar
          >> spawnOnce wallpaper
          >> spawnOnce compositor
          >> spawnOnce xresource
          >> spawnOnce cursor
          >> spawnOnce "flashfocus"
    }

promptConfig :: P.XPConfig
promptConfig =
  P.def
    { P.fgColor = "#f55966",
      P.bgColor = "#090909",
      P.font = "VictorMono Nerd Font Mono",
      P.promptBorderWidth = 0,
      P.height = 52,
      P.defaultText = "",
      P.historySize = 0,
      P.maxComplRows = Just 0,
      P.position = P.Top
    }

scratchPadFilter :: WorkspaceSort
scratchPadFilter = filterOutWs [scratchpadWorkspaceTag]
