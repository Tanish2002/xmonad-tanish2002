module Config.Options where

-- Imports --------------------------------------------------------------------
import           XMonad

import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Decoration
import           XMonad.Util.Font
import           XMonad.Util.SpawnOnce

import           Apps.Alias

-- Types ------------------------------------------------------------------------
data Options = Options
  { term       :: String
  , ffm        :: Bool
  , clickfocus :: Bool
  , border     :: Dimension
  , modkey     :: KeyMask
  , spaces     :: [String]
  , autoload   :: X ()
  }
-- Functions --------------------------------------------------------------------
options :: Options
options = Options
  { term   = myterm
  , ffm    = False
  , clickfocus = False
  , border = 0
  , modkey   = mod4Mask
  , spaces = [ "\xf269", "\xe795", "\xfb8a", "\xf044", "\xf675", "\xfb6e" ]
  , autoload = setWMName "XMonad"
             >> spawnOnce battery
             >> spawnOnce bar
             >> spawnOnce wallpaper
             >> spawnOnce cursor
             >> spawnOnce xresource
  }
