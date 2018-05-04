import XMonad
import XMonad.Util.EZConfig
import XMonad.StackSet

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect

import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad

import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.GroupNavigation
import XMonad.Actions.FloatKeys

import System.Exit
import Data.Maybe
import Control.Monad (when)
import qualified Data.Map as M

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

customTabTheme = (theme xmonadTheme)
  { fontName      = "xft:Iosevka Medium-12"
  , decoHeight    = 20
  , activeTextColor     = "#222222"
  , activeColor         = "#909737"
  , inactiveTextColor   = "#999999"
  , inactiveColor       = "#161616"
  , activeBorderColor   = "#909737"
  , inactiveBorderColor = "#161616" }

availableLayouts = smartBorders $ tabs ||| tilesLM ||| tilesRM ||| tilesTM ||| tilesBM
  where
    tabs    = tabbed shrinkText customTabTheme
    tilesLM = Tall 1 delta ratio
    tilesRM = reflectHoriz tilesLM
    tilesTM = Mirror tilesLM
    tilesBM = reflectVert tilesTM
    ratio   = 1/2
    delta   = 3/100

windowBringerDmenuConfig = def { menuCommand  = "rofi"
                               , menuArgs     = [ "-p", "win", "-dmenu", "-i" ] }

floatRectFull   = hideScreenBorder $ RationalRect 0      0      1       1
floatRectLarge  = hideScreenBorder $ RationalRect (1/20) (1/20) (18/20) (18/20)
floatRectCenter = hideScreenBorder $ RationalRect (1/6)  (1/6)  (2/3)   (2/3)
floatRectBottom = hideScreenBorder $ RationalRect (1/20) (1/3)  (18/20) (2/3)
floatRectTop    = hideScreenBorder $ RationalRect (1/20) 0      (18/20) (2/3)
floatRectLeft   = hideScreenBorder $ RationalRect 0      (1/20) (1/2)   (18/20)
floatRectRight  = hideScreenBorder $ RationalRect (1/2)  (1/20) (1/2)   (18/20)

scratchpads = [ NS "terminal" "kitty --class=scratchterm" (className =? "scratchterm")
                   (customFloating floatRectTop)
              , NS "browser" "firefox" (className =? "Firefox")
                   (customFloating floatRectTop)
              , NS "documentation" "zeal" (className =? "Zeal")
                   (customFloating floatRectLarge)
              , NS "messaging" "telegram-desktop" (className =? "TelegramDesktop")
                   (customFloating floatRectRight) ]

keybindings =
-- xmonad session control
  [ ("C-M1-<Escape>"    , io (exitWith ExitSuccess))
  , ("C-M1-<Backspace>" , spawn "xmonad --restart")
-- application launchers
  , ("M-<Space>"     , spawn "rofi -show combi")
  , ("M-<Return>"    , spawn "kitty")
  , ("M-S-<Return>"  , spawn "vim -g")
-- window management
  , ("M-q"           , windows $ shift "NSP")
  , ("M-S-q"         , kill)
  , ("M-j"           , windows focusDown)
  , ("M-k"           , windows focusUp)
  , ("M-S-j"         , windows swapDown)
  , ("M-S-k"         , windows swapUp)
  , ("M-h"           , sendMessage Shrink)
  , ("M-l"           , sendMessage Expand)
  , ("M-<Backspace>" , nextMatch History (return True))
-- window bringer
  , ("M-a"           , gotoMenuConfig  windowBringerDmenuConfig)
  , ("M-S-a"         , bringMenuConfig windowBringerDmenuConfig)
-- scratchpads
  , ("M-b"           , namedScratchpadAction scratchpads "browser")
  , ("M-d"           , namedScratchpadAction scratchpads "documentation")
  , ("M-m"           , namedScratchpadAction scratchpads "messaging") ] ++
-- workspace selection
  [ (p ++ [k]        , windows $ f i) | (i, k) <- zip Main.workspaces ['1' .. '9']
                                      , (p, f) <- [ ("M-"   , greedyView)
                                                  , ("M-S-" , shift) ] ] ++
-- workspace management
  [ ("M-s l"         , sendMessage NextLayout)
  , ("M-s p"         , toggleWS' ["NSP"])
  , ("M-s j"         , moveTo  Next nonEmptyWS)
  , ("M-s k"         , moveTo  Prev nonEmptyWS)
  , ("M-S-s j"       , shiftTo Next nonEmptyWS >> moveTo Next nonEmptyWS)
  , ("M-S-s k"       , shiftTo Prev nonEmptyWS >> moveTo Prev nonEmptyWS)
-- floating placement
  , ("M-w t"         , withFocused $ windows . sink)
  , ("M-w f"         , withFocused $ placeFloating floatRectFull )
  , ("M-w S-c"       , withFocused $ placeFloating floatRectLarge)
  , ("M-w c"         , withFocused $ placeFloating floatRectCenter)
  , ("M-w j"         , withFocused $ placeFloating floatRectBottom)
  , ("M-w k"         , withFocused $ placeFloating floatRectTop)
  , ("M-w h"         , withFocused $ placeFloating floatRectLeft)
  , ("M-w l"         , withFocused $ placeFloating floatRectRight)
-- system control
  , ("M-c <Up>"      , spawn "amixer sset Master 10%+")
  , ("M-c <Down>"    , spawn "amixer sset Master 10%-")
  , ("M-c m"         , spawn "amixer sset Master toggle") ]

customEventHook = do
  handleEventHook defaultConfig
  fullscreenEventHook

customLogHook = do
  historyHook
  customizeBorderWhen (isFloat <&&> isNotFullscreen) "#aadb0f" 6

main = xmonad $ ewmh
              $ defaultConfig
  { modMask             = mod4Mask -- super key as modifier
  , borderWidth         = 3
  , normalBorderColor   = "#161616"
  , focusedBorderColor  = "#909737"
  , keys                = \c -> mkKeymap c keybindings
  , startupHook         = return () >> checkKeymap defaultConfig keybindings
  , handleEventHook     = customEventHook
  , layoutHook          = availableLayouts
  , manageHook          = namedScratchpadManageHook scratchpads
  , logHook             = customLogHook }
  `additionalKeys`
  [ ((noModMask, xK_Menu) , namedScratchpadAction scratchpads "terminal") ]

nonEmptyWS = WSIs $ return (\w -> nonNSP w && nonEmpty w)
  where nonNSP (Workspace tag _ _) = tag /= "NSP"
        nonEmpty = isJust . stack

placeFloating :: RationalRect -> Window -> X ()
placeFloating rect = windows . (flip XMonad.StackSet.float $ rect)

windowSize w = do
  r <- withDisplay $ (\d -> io $ getWindowAttributes d w)
  return (fromIntegral $ wa_width r, fromIntegral $ wa_height r)

withCurrentScreen     f = withWindowSet     $ \ws -> f (current ws)
withCurrentScreenRect f = withCurrentScreen $ \s  -> f (screenRect (screenDetail s))

screenResolution = withCurrentScreenRect $ \r -> return (rect_width r, rect_height r)

isNotFullscreen :: Query Bool
isNotFullscreen = ask >>= (\w -> liftX $ do (ww, wh) <- windowSize w
                                            (sw, sh) <- screenResolution
                                            return $ not (ww == sw && wh == sh))

isFloat :: Query Bool
isFloat = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ (M.member w (floating ws)))

customizeBorderWhen :: Query Bool -> String -> Dimension -> X ()
customizeBorderWhen q color width = withFocused $ \w -> runQuery q w >>= flip when (setWindowBorder' color width w)

setWindowBorder' :: String -> Dimension -> Window -> X ()
setWindowBorder' color width window = do
  XConf { display = d } <- ask
  ~(Just pixel) <- io $ initColor d color
  io $ setWindowBorder      d window pixel
  io $ setWindowBorderWidth d window width

-- ugly hack to hide window border at screen boundary
hideScreenBorder :: RationalRect -> RationalRect
hideScreenBorder (RationalRect x0 y0 w h) = RationalRect (x0-(bw/sw)) (y0-(bw/sh)) (w+((2*bw)/sw)) (h+((2*bw+1)/sh))
  where bw = 6
        sw = 1280
        sh = 1024
