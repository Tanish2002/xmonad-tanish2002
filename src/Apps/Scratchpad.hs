module Apps.Scratchpad where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)

-- scratchpads :: String
scratchpads :: [NamedScratchpad]
scratchpads =
  [NS "scratchpad" "st -t scratchpad" (title =? "scratchpad") (customFloating (W.RationalRect l t w h))]
  where
    h = 1.00 -- height: 20%
    w = 0.29 -- width
    t = 0.00 -- distance from top
    l = 0.00 -- distance from left
