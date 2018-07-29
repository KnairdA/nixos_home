import XMonad hiding ((|||))
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition

import XMonad.Layout.Tabbed
import XMonad.Layout.MultiColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.OneBig

import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.LayoutCombinators

import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad
import XMonad.Util.Dmenu (menuMapArgs)

import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.GroupNavigation
import XMonad.Actions.FloatKeys

import Data.Maybe
import Control.Monad (when)
import Data.Map (Map, fromList, member)

import System.Exit
import System.Posix.Unistd

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

customTabTheme = (theme xmonadTheme)
  { fontName      = "xft:Iosevka Medium-12"
  , decoHeight    = 20
  , activeTextColor     = "#222222"
  , activeColor         = "#909636"
  , inactiveTextColor   = "#999999"
  , inactiveColor       = "#161616"
  , activeBorderColor   = "#909636"
  , inactiveBorderColor = "#161616" }

availableLayouts = id
  . smartBorders
  . mkToggle (single NBFULL)
  $ tabs ||| tiles ||| two ||| frame
  where
    tabs   = name "tabs"  $ tabbed shrinkText customTabTheme
    tiles  = name "tiles" $ id
                          . mkToggle (single REFLECTX)
                          . mkToggle (single REFLECTY)
                          $ multiCol [1, 2, 0] 1 delta (1/3)
    two    = name "two"   $ TwoPane delta (1/2)
    frame  = name "frame" $ id
                          . mkToggle (single REFLECTX)
                          . mkToggle (single REFLECTY)
                          . reflectVert
                          $ OneBig (2/3) (4/5)
    delta  = 1/24
    name n = renamed [Replace n]

-- layout names for layout selection dialog
layoutNames = fromList [ ("Tabs"                    , "tabs")
                       , ("Multi-column tiles"      , "tiles")
                       , ("Two columns"             , "two")
                       , ("One large framed window" , "frame") ]

floatRectTop    h = S.RationalRect (1/20) 0      (18/20) h
floatRectBottom h = S.RationalRect (1/20) (1-h)  (18/20) h
floatRectLeft   w = S.RationalRect 0      (1/20) w       (18/20)
floatRectRight  w = S.RationalRect (1-w)  (1/20) w       (18/20)

dropUp        = floatRectBottom $ 2/3
dropUpLarge   = floatRectBottom $ 18/20
dropDown      = floatRectTop    $ 2/3
dropDownLarge = floatRectTop    $ 18/20
sideBarLeft   = floatRectLeft   $ 1/3
sideBarRight  = floatRectRight  $ 1/3

scratchpads host =
  [ NS "terminal"      "kitty --class=scratchterm"                             (className =? "scratchterm")
       (customFloating $ hideScreenBorder host dropDown)
  , NS "browser"       "firefox --no-remote -P scratchpad --class scratchfire" (className =? "scratchfire")
       (customFloating $ hideScreenBorder host dropDownLarge)
  , NS "documentation" "zeal"                                                  (className =? "Zeal")
       (customFloating $ hideScreenBorder host dropDown)
  , NS "messaging"     "telegram-desktop"                                      (className =? "TelegramDesktop")
       (customFloating $ hideScreenBorder host sideBarRight) ]

windowBringerDmenuConfig = def { menuCommand  = "rofi"
                               , menuArgs     = [ "-p", "win", "-dmenu", "-i" ] }

hostSpecificKeybindings host = case host of
  "asterix" -> [ ("M-i b" , showNotification "Battery"
                                             "`acpi | cut -c 10-`")
               , ("M-i c" , showNotification "`acpi --thermal | awk '{print $4}'`°C"
                                             "`cat /proc/acpi/ibm/fan | awk '/speed/{print $2}'` RPM") ]
  "obelix"  -> [ ("M-i g" , showNotification "GPU"
                                             "`nvidia-smi --query-gpu=name,temperature.gpu,utilization.gpu,utilization.memory --format=csv,noheader | awk -F',' '{print $1 \" running at\" $2 \"°C due to\" $3 \" load and\" $4 \" memory usage\"}'`") ]
  _         -> [ ]

commonKeybindings host =
-- xmonad session control
  [ ("C-M1-<Escape>"    , io (exitWith ExitSuccess))
  , ("C-M1-<Backspace>" , spawn "xmonad --restart")
  , ("C-M1-l"           , spawn "i3lock -c 000000")
-- application launchers
  , ("M-<Space>"     , spawn "rofi -show combi")
  , ("M-<Return>"    , spawn "kitty")
  , ("M-S-<Return>"  , spawn "nvim-qt")
  , ("<Print>"       , spawn "xfce4-screenshooter")
-- window management
  , ("M-q"           , windows $ S.shift "NSP")
  , ("M-S-q"         , kill)
  , ("M-j"           , windows S.focusDown)
  , ("M-k"           , windows S.focusUp)
  , ("M-S-j"         , windows S.swapDown)
  , ("M-S-k"         , windows S.swapUp)
  , ("M-h"           , sendMessage Shrink)
  , ("M-l"           , sendMessage Expand)
  , ("M-<Backspace>" , nextMatch History (return True))
-- window bringer
  , ("M-a"           , gotoMenuConfig  windowBringerDmenuConfig)
  , ("M-S-a"         , bringMenuConfig windowBringerDmenuConfig)
-- scratchpads
  , ("M-b"           , namedScratchpadAction (scratchpads host) "browser")
  , ("M-d"           , namedScratchpadAction (scratchpads host) "documentation")
  , ("M-m"           , namedScratchpadAction (scratchpads host) "messaging") ] ++
-- workspace selection
  [ (p ++ [k]        , windows $ f i) | (i, k) <- zip Main.workspaces ['1' .. '9']
                                      , (p, f) <- [ ("M-"   , S.greedyView)
                                                  , ("M-S-" , S.shift) ] ] ++
  [ ("M-s p"         , toggleWS' ["NSP"])
-- workspace movement
  , ("M-s j"         , moveTo  Next nonEmptyWS)
  , ("M-s k"         , moveTo  Prev nonEmptyWS)
  , ("M-S-s j"       , shiftTo Next nonEmptyWS >> moveTo Next nonEmptyWS)
  , ("M-S-s k"       , shiftTo Prev nonEmptyWS >> moveTo Prev nonEmptyWS)
-- workspace layout management
  , ("M-v"           , layoutMenu)
  , ("M-s l"         , sendMessage NextLayout)
  , ("M-s +"         , sendMessage $ IncMasterN   1)
  , ("M-s -"         , sendMessage $ IncMasterN (-1))
  , ("M-s y"         , sendMessage $ Toggle REFLECTY)
  , ("M-s x"         , sendMessage $ Toggle REFLECTX)
  , ("M-s f"         , sendMessage $ Toggle NBFULL)
-- floating placement
  , ("M-w t"         , withFocused $ windows . S.sink)
  , ("M-w j"         , withFocused $ placeFloating host dropUp)
  , ("M-w S-j"       , withFocused $ placeFloating host dropUpLarge)
  , ("M-w k"         , withFocused $ placeFloating host dropDown)
  , ("M-w S-k"       , withFocused $ placeFloating host dropDownLarge)
  , ("M-w h"         , withFocused $ placeFloating host sideBarLeft)
  , ("M-w l"         , withFocused $ placeFloating host sideBarRight)
-- system information
  , ("M-i t"         , showNotification "`date +%T`" "`date +\"%Y-%m-%d\"`")
  , ("M-i l"         , showNotification "Load" "`cut -c -14 /proc/loadavg`")
-- system control
  , ("M-c <Up>"      , spawn "amixer sset Master 10%+")
  , ("M-c <Down>"    , spawn "amixer sset Master 10%-")
  , ("M-c m"         , spawn "amixer sset Master toggle") ]

customKeybindings host = concatMap ($ host) [commonKeybindings, hostSpecificKeybindings]

customMousebindings (XConfig {XMonad.modMask = modMask}) = fromList
  [ ((modMask .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w)
  , ((modMask .|. shiftMask, button3), \w -> focus w >> mouseResizeWindow w) ]

customEventHook = do
  handleEventHook def
  fullscreenEventHook

customManageHook host = composeOne
  [ hasRole "GtkFileChooserDialog" -?> doRectFloat dropDown
  , isDialog                       -?> doCenterFloat
  , transience
  , pure True -?> insertPosition Below Newer <+> namedScratchpadManageHook (scratchpads host) ]
  where
    hasRole x = stringProperty "WM_WINDOW_ROLE" =? x

customLogHook = do
  historyHook
  customizeBorderWhen (isFloat <&&> isNotFullscreen) "#aadb0f" 6

main = do
  host <- fmap nodeName getSystemID
  xmonad $ ewmh
         $ def
    { modMask             = mod4Mask -- super key as modifier
    , borderWidth         = 3
    , normalBorderColor   = "#161616"
    , focusedBorderColor  = "#909636"
    , keys                = \c -> mkKeymap c (customKeybindings host)
    , mouseBindings       = customMousebindings
    , startupHook         = return () >> checkKeymap def (customKeybindings host)
    , handleEventHook     = customEventHook
    , manageHook          = customManageHook host
    , logHook             = customLogHook
    , layoutHook          = availableLayouts }
    `additionalKeys`
    [ ((noModMask, xK_Menu) , namedScratchpadAction (scratchpads host) "terminal") ]

nonEmptyWS = WSIs $ return (\w -> nonNSP w && nonEmpty w)
  where nonNSP (S.Workspace tag _ _) = tag /= "NSP"
        nonEmpty = isJust . S.stack

showNotification title text = spawn ("notify-send \"" ++ title ++ "\" \"" ++ text ++ "\"")

-- layout selection dialog
layoutMenu :: X ()
layoutMenu = (askUserForLayout layoutNames) >>= setLayoutByName
  where
    setLayoutByName :: (Maybe String) -> X ()
    setLayoutByName value = case value of
      Just name -> sendMessage (JumpToLayout name)
      Nothing   -> return ()
    askUserForLayout :: Map String String -> X (Maybe String)
    askUserForLayout = menuMapArgs "rofi" [ "-p", "layout", "-dmenu", "-i" ]


-------------------------------------------------------------------------------
-- utilities for customizing borders of floating windows

withCurrentScreen     f = withWindowSet     $ \ws -> f (S.current ws)
withCurrentScreenRect f = withCurrentScreen $ \s  -> f (screenRect (S.screenDetail s))

screenResolution = withCurrentScreenRect $ \r -> return (rect_width r, rect_height r)

windowSize w = do
  r <- withDisplay $ (\d -> io $ getWindowAttributes d w)
  return (fromIntegral $ wa_width r, fromIntegral $ wa_height r)

isNotFullscreen :: Query Bool
isNotFullscreen = ask >>= (\w -> liftX $ do (ww, wh) <- windowSize w
                                            (sw, sh) <- screenResolution
                                            return $ not (ww == sw && wh == sh))

isFloat :: Query Bool
isFloat = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ (member w (S.floating ws)))

customizeBorderWhen :: Query Bool -> String -> Dimension -> X ()
customizeBorderWhen q color width = withFocused $ \w -> runQuery q w >>= flip when (setWindowBorder' color width w)

setWindowBorder' :: String -> Dimension -> Window -> X ()
setWindowBorder' color width window = do
  XConf { display = d } <- ask
  ~(Just pixel) <- io $ initColor d color
  io $ setWindowBorder      d window pixel
  io $ setWindowBorderWidth d window width

-------------------------------------------------------------------------------
-- ugly hack to hide window border at screen boundary

placeFloating :: String -> S.RationalRect -> Window -> X ()
placeFloating host rect = windows . (flip S.float $ (hideScreenBorder host rect))

hideScreenBorder :: String -> S.RationalRect -> S.RationalRect
hideScreenBorder host (S.RationalRect x0 y0 w h) = S.RationalRect (x0-(bw/sw)) (y0-(bw/sh)) (w+((2*bw)/sw)) (h+((2*bw+1)/sh))
  where bw = 6
        sw = screenWidthOn  host
        sh = screenHeightOn host

screenWidthOn  host = case host of
  "obelix"  -> 1920
  "asterix" -> 1280
screenHeightOn host = case host of
  "obelix"  -> 1200
  "asterix" -> 768
