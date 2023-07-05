module Hooks.EventHook
  ( events,
  )
where

-- Imports --------------------------------------------------------------------

import Data.Monoid (All)
import Data.Functor
import XMonad
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.BorderPerWindow (defineBorderWidth)
import XMonad.Layout.Fullscreen as FS

-- EventHook --------------------------------------------------------------------
events :: Event -> X All
events =
  mconcat
    [ FS.fullscreenEventHook,
      swallowEventHook (title =? "st") (return True),
      (dynamicTitle <> windowCloseHook) (title >>= \t -> defineBorderWidth (if t == "st" then 45 else 0))
    ]

windowCloseHook :: ManageHook -> Event -> X All
windowCloseHook mh DestroyWindowEvent{ ev_event = w0, ev_window = w } | w0 == w = runQuery mh w $> mempty
windowCloseHook _ _ = mempty
