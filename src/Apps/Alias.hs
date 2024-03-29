-- | Personal 'programs as functions' list
-- This is here so that I can change the programs behind things without worry
-- about propogating all the changes. All functions external to haskell should
-- be specified here.
module Apps.Alias where

battery :: String
battery = "~/bin/batterywarn"

compositor :: String
compositor = "picom --config ~/.config/picom/picom.conf"

bar :: String
bar = "~/.config/polybar/launch.sh"

wallpaper :: String
wallpaper = "~/.fehbg"

cursor :: String
cursor = "xsetroot -cursor_name left_ptr &"

xresource :: String
xresource = "xrdb -remove && xrdb $HOME/xrdb/cute/cute"

myterm :: String
myterm = "st"
