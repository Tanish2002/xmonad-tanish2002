module Bind.KeyBoard
  ( keyboard,
  )
where

-- Imports --------------------------------------------------------------------

import Apps.Alias
import Apps.Scratchpad (scratchpads)
import Config.Options
import Container.Layout
import qualified Data.Map as M
import Data.Maybe (Maybe, fromJust, isNothing)
import Data.Ratio
import XMonad
import XMonad.Actions.FloatKeys
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.SubLayouts
import XMonad.Prompt.Theme
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

-- Bindings --------------------------------------------------------------------

keyboard :: [(String, X ())]
keyboard = concat [customBindings, wmBindings, multimediaBindings]
  where
    customBindings :: [(String, X ())]
    customBindings =
      [ -- launch terminal
        ("M-<Return>", spawn (term options)),
        -- launch dmenu
        ("M-d", spawn "rofi -show drun"),
        ("M-S-d", spawn "rofi -show run"),
        -- launch rofi-emoji
        ("M-e", spawn "$HOME/bin/rofi-emoji"),
        -- launch rofi-mpvtube
        ("M-m", spawn "$HOME/bin/rofi_mpvtube"),
        -- launch rofi-clip
        ("M-c", spawn "$HOME/bin/rofi-clip"),
        ("M-q", themePrompt def)
      ]

    wmBindings :: [(String, X ())]
    wmBindings =
      [ -- close focused window
        ("M-S-q", kill),
        -- Rotate through the available layout algorithms
        ("M-<Space>", sendMessage NextLayout),
        -- Resize viewed windows to the correct size
        ( "M-n",
          refresh
            >> spawn "flash_window"
        ),
        -- Move focus to the next/previous window
        ("M-<Tab>", windows W.focusDown),
        ("M-S-<Tab>", windows W.focusUp),
        -- Move focus to the next window
        ("M-j", windows W.focusDown),
        ("M-<Down>", windows W.focusDown),
        -- Move focus to the previous window
        ("M-k", windows W.focusUp),
        ("M-<Up>", windows W.focusUp),
        -- Move focus to the master window
        ("M-S-m", windows W.focusMaster),
        -- Swap the focused window and the master window
        ("M-S-<Return>", windows W.swapMaster),
        -- Swap the focused window with the next window
        ("M-S-j", windows W.swapDown),
        ("M-S-<Down>", windows W.swapDown),
        -- Swap the focused window with the previous window
        ("M-S-k", windows W.swapUp),
        ("M-S-<Up>", windows W.swapUp),
        -- Float Window Control
        ("M1-w", withFocused (keysMoveWindow (0, -10))),
        ("M1-a", withFocused (keysMoveWindow (-10, 0))),
        ("M1-s", withFocused (keysMoveWindow (0, 10))),
        ("M1-d", withFocused (keysMoveWindow (10, 0))),
        ("M1-S-w", withFocused (keysResizeWindow (10, 10) (1 % 2, 1 % 2))),
        ("M1-S-s", withFocused (keysResizeWindow (-10, -10) (1 % 2, 1 % 2))),
        -- Tabbed Layout Control
        ("M1-t", sendMessage $ Toggle ENABLETABS),
        ("M1-S-<Left>", sendMessage $ pullGroup L),
        ("M1-S-<Down>", sendMessage $ pullGroup D),
        ("M1-S-<Up>", sendMessage $ pullGroup U),
        ("M1-S-<Right>", sendMessage $ pullGroup R),
        ("M1-m", withFocused (sendMessage . MergeAll)),
        ("M1-u", withFocused (sendMessage . UnMerge)),
        -- Shrink the master area
        ("M-h", sendMessage Shrink),
        ("M-<Left>", sendMessage Shrink),
        -- Expand the master area
        ("M-l", sendMessage Expand),
        ("M-<Right>", sendMessage Expand),
        -- Toggle between Float and Tile
        ("M-t", withFocused toggleFloat),
        -- Increment the number of windows in the master area
        ("M-,", sendMessage (IncMasterN 1)),
        -- Deincrement the number of windows in the master area
        ("M-.", sendMessage (IncMasterN (-1))),
        -- Toggle the status bar gap
        -- Use this binding with avoidStruts from Hooks.ManageDocks.
        -- See also the statusBar function from Hooks.DynamicLog.
        --
        ( "M-b",
          do
            sendMessage ToggleStruts
            spawn "polybar-msg cmd toggle"
        ),
        -- Quit xmonad
        ("M-S-<Escape>", spawn "pm"),
        -- Restart xmonad
        ("M-S-r", spawn "xmonad --restart"),
        -- Spawn Scratchpad
        ("M-`", namedScratchpadAction scratchpads "scratchpad")
      ]
        ++ [ ("M-\\ " ++ k, S.promptSearch promptConfig f)
             | (k, f) <- searchList
           ]
        ++ [ ("M" ++ m ++ ['-', k], windows $ f i)
             | (i, k) <- zip (spaces options) ['1' .. '9'],
               (f, m) <- [(W.view, ""), (W.shift, "-S")]
           ]

    multimediaBindings :: [(String, X ())]
    multimediaBindings =
      [ ("<XF86AudioPlay>", spawn "$HOME/bin/mpvctl -t"),
        ("<XF86AudioPrev>", spawn "pkill skroll ; $HOME/bin/mpvctl -p"),
        ("<XF86AudioNext>", spawn "pkill skroll ; $HOME/bin/mpvctl -n"),
        ("<XF86AudioMute>", spawn "amixer set Master toggle"),
        ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"),
        ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"),
        ("<XF86MonBrightnessUp>", spawn "light -A 10"),
        ("<XF86MonBrightnessDown>", spawn "light -U 10"),
        ("<Print>", spawn "$HOME/bin/screenshot-menu.sh")
      ]

-- search engine submap
searchList :: [(String, S.SearchEngine)]
searchList = [("g", S.duckduckgo), ("d", S.dictionary), ("w", S.wikipedia), ("y", S.youtube), ("m", S.multi)]

skipFloating :: (Eq a, Ord a) => W.StackSet i l a s sd -> (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd
skipFloating stacks f
  | isNothing curr = stacks -- short circuit if there is no currently focused window
  | otherwise = skipFloatingR stacks curr f
  where
    curr = W.peek stacks

skipFloatingR :: (Eq a, Ord a) => W.StackSet i l a s sd -> Maybe a -> (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd
skipFloatingR stacks startWindow f
  | isNothing nextWindow = stacks -- next window is nothing return current stack set
  | nextWindow == startWindow = newStacks -- if next window is the starting window then return the new stack set
  | M.notMember (fromJust nextWindow) (W.floating stacks) = newStacks -- if next window is not a floating window return the new stack set
  | otherwise = skipFloatingR newStacks startWindow f -- the next window is a floating window so keep recursing (looking)
  where
    newStacks = f stacks
    nextWindow = W.peek newStacks

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
    )

isFloat :: Window -> X Bool
isFloat w = do
  fls <- withWindowSet (return . W.floating)
  return (w `M.member` fls)
