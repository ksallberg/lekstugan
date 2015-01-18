module Main (main) where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks

type MyKeys = [((ButtonMask, KeySym), X ())]

myKeys :: MyKeys
myKeys = [ ((mod4Mask, xK_Return), spawn "terminator")
         , ((mod4Mask, xK_w),      spawn "google-chrome")
         , ((mod4Mask, xK_f),      spawn "nautilus")
         , ((mod4Mask, xK_Left),   prevWS)
         , ((mod4Mask, xK_Right),  nextWS)
         , ((mod4Mask, xK_s),      spawn "skype")
         ]

{-
    "code" followed by undef1 -> undef8
-}
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["Code", "Media"] ++ (map (\x->"Screen "++show x) [1..7])

myLayout = avoidStruts $
           tiled
           ||| reflectHoriz tiled
           ||| Mirror tiled
           ||| Full
           ||| Circle
--           ||| tabbed shrinkText defaultTheme
--           ||| threeCol
--           ||| spiral (4/3)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     threeCol = ThreeCol nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100

mediaLayout :: ModifiedLayout WithBorder Full Window
mediaLayout = noBorders Full

myLayouts = onWorkspace "Media" mediaLayout myLayout

main :: IO ()
main = do
    xmonad $ gnomeConfig { workspaces         = myWorkspaces
                         , layoutHook         = myLayout
                         , terminal           = "terminator"
                         , modMask            = mod4Mask
                         , normalBorderColor  = "#000000"
                         , focusedBorderColor = "#ADD8E6"
                         } `additionalKeys` myKeys
