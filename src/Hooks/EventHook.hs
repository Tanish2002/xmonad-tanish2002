module Hooks.EventHook
  ( events,
  )
where

-- Imports --------------------------------------------------------------------

import Data.Monoid (All)
import XMonad
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Layout.Fullscreen as FS

-- EventHook --------------------------------------------------------------------
events :: Event -> X All
events =
  mconcat
    [ FS.fullscreenEventHook,
      swallowEventHook (className =? "St") (return True)
    ]
