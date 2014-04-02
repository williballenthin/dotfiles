import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Input
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.IO

import XMonad.Hooks.ICCCMFocus

queryBuilder :: String -> String
queryBuilder query = "query=$(echo \"" ++ query ++ "\" | sed -e \"s/ /+/g\") && dwb \"www.google.com/search?q=$query\""

queryPrompt :: XPConfig -> X ()
queryPrompt c =
    inputPrompt c "query" ?+ \query ->
    spawn (queryBuilder query)
    >> return()


myWorkspaces = ["1:dev","2:term","3","4:web","5","6","7:mail","8:chat","9","0","-","="]
myManageHook = composeAll
    [ className =? "Firefox" --> doShift "4:web"
    , resource =? "desktop_window" --> doIgnore
    , resource =? "gpicview" --> doFloat
    , className =? "MPlayer" --> doFloat
    , className =? "Thunderbird" --> doShift "7:mail"
    , className =? "skype" --> doShift "8:chat"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]



keysToAdd x =
    [ ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((mod4Mask, xK_s), withFocused $ windows . W.sink)
    , ((mod4Mask, xK_g), queryPrompt defaultXPConfig)
    , ((mod4Mask, xK_t), spawn "urxvt -pe tabbed")
    , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 5%+")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 5%-")
    ]
keysToDel x =
    [ (mod4Mask, xK_t)
    ]

myLayout = avoidStruts (noBorders Full
                    ||| ThreeCol 1 (3/100) (1/2)
                    ||| spiral (6/7)
                    ||| tiled
                    ||| (tabbed shrinkText myTConf))
    where
        tiled = smartSpacing 15 $ Tall nmaster delta ratio
        nmaster = 1
        ratio = 4/6
        delta = 3/100
        myTConf = defaultTheme { decoHeight = 12 }

keysDefault = keys defaultConfig
keysStrip x = foldr M.delete            (keysDefault x) (keysToDel x)
myKeys x    = foldr (uncurry M.insert)  (keysStrip x)   (keysToAdd x)

main = do
     xmonad $ defaultConfig
       { manageHook = manageDocks <+> myManageHook
       , layoutHook = myLayout
       , startupHook = setWMName "LG3D"
       , workspaces = myWorkspaces
       , modMask = mod4Mask
       , keys = myKeys
       , normalBorderColor = "#332d29"
       , focusedBorderColor = "#817267"
       , borderWidth = 2
       , logHook = takeTopFocus
       }

