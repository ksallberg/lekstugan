module Main (main) where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowNavigation
import XMonad.Config.Gnome
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig

type MyKeys = [((ButtonMask, KeySym), X ())]

myNormalBorderColor :: String
myNormalBorderColor  = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ADD8E6"

myKeys :: MyKeys
myKeys = [ ((mod4Mask, xK_Return), spawn "terminator")
         , ((mod4Mask, xK_w),      spawn "google-chrome")
         , ((mod4Mask, xK_f),      spawn "nautilus")
         , ((mod4Mask, xK_Left),   prevWS)
         , ((mod4Mask, xK_Right),  nextWS)
         , ((mod4Mask, xK_s),      spawn "skype")
         ]

main :: IO ()
main = do
    xmonad $ gnomeConfig { terminal           = "terminator"
                         , modMask            = mod4Mask
                         , normalBorderColor  = myNormalBorderColor
                         , focusedBorderColor = myFocusedBorderColor
                         } `additionalKeys` myKeys
