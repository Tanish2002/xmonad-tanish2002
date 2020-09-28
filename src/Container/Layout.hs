module Container.Layout where

-- Imports --------------------------------------------------------------------
import XMonad                  

import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.SimpleFloat
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import Data.Maybe
import XMonad.Layout.BinarySpacePartition
import Control.Monad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders

import XMonad.StackSet               as W


------------------------------------------------------------------------
--
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- Types ------------------------------------------------------------------------
data Gaps' = Gaps'
  { u  :: Integer
  , d  :: Integer
  , r  :: Integer
  , l  :: Integer
  }

-- Functions --------------------------------------------------------------------

gs :: Gaps'
gs = Gaps' { u = 20, d = 20, r = 20, l = 20 }

spacingses :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
spacingses = spacingRaw False
             (Border (u gs) 0 (r gs) 0)
             True
             (Border 0 (d gs) 0 (l gs))
             True

outerGaps    = 20
myGaps       = gaps [(U, outerGaps), (R, outerGaps), (L, outerGaps), (D, outerGaps)]
-- customised layouts

full = named "Fullscreen" $ noBorders (fullscreenFull Full)

tall =
  named "Tall"
    . spacingses
    $ ResizableTall 1 (2 / 100) (1 / 2) []

tabs = named "Tabbed"
  . myGaps
  $ tabbed shrinkText tabTheme

bsp =
  named "Binary Partition"
    . spacingses
    $ emptyBSP

float' = named "Float" $ simpleFloat' shrinkText emptyTheme

-- layout --
layout = fullscreenFull $  avoidStruts $ (bsp ||| tabs ||| float' ||| tall ||| Mirror tall ||| full)

tabTheme :: Theme
tabTheme = def { activeColor         = "#090909"
               , activeBorderColor   = "#000000"
               , activeTextColor     = "#F2A5B7"
               , inactiveColor       = "#F2A5B7"
               , inactiveBorderColor = "#000000"
               , inactiveTextColor   = "#090909"
               , urgentColor         = "#090909"
               , urgentBorderColor   = "#000000"
               , urgentTextColor     = "#F2A5B7"
               , fontName            = "VictorMono Nerd Font Mono"
               , decoHeight          = 52
               }

emptyTheme :: Theme
emptyTheme = def { decoHeight = 0, decoWidth = 0 }

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]
