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

import System.Exit
import Data.Maybe

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

floatRectFull   = RationalRect (1/20) (1/20) (18/20) (18/20)
floatRectCenter = RationalRect (1/6)  (1/6)  (2/3)   (2/3)
floatRectBottom = RationalRect 0      (1/3)  1       (2/3)
floatRectTop    = RationalRect 0      0      1       (2/3)
floatRectLeft   = RationalRect 0      0      (1/2)   1
floatRectRight  = RationalRect (1/2)  0      (1/2)   1

scratchpads = [ NS "terminal" "kitty --class=scratchterm" (className =? "scratchterm")
                   (customFloating floatRectCenter)
              , NS "browser" "firefox" (className =? "Firefox")
                   (customFloating floatRectTop)
              , NS "zeal" "zeal" (className =? "Zeal")
                   (customFloating floatRectFull)
              , NS "telegram" "telegram-desktop" (className =? "TelegramDesktop")
                   (customFloating floatRectCenter) ]

main = xmonad $ ewmh
              $ defaultConfig
  { modMask             = mod4Mask -- super key as modifier
  , borderWidth         = 3
  , normalBorderColor   = "#161616"
  , focusedBorderColor  = "#909737"
  , terminal            = "kitty"
  , handleEventHook     = handleEventHook defaultConfig <+> fullscreenEventHook
  , layoutHook          = availableLayouts
  , manageHook          = namedScratchpadManageHook scratchpads
  , logHook             = historyHook }
  `removeKeysP`
  [ "M-S-<Return>", "M-q", "M-S-q", "M-S-c", "M-<Space>" ]
  `additionalKeysP`
-- xmonad session control
  [ ("C-M1-<Escape>"    , io (exitWith ExitSuccess))
  , ("C-M1-<Backspace>" , spawn "xmonad --restart")
-- application launchers
  , ("M-p"           , spawn "rofi -show combi")
  , ("M-<Return>"    , spawn "kitty")
  , ("M-S-<Return>"  , spawn "vim -g")
-- window management
  , ("M-q"           , windows $ shift "NSP")
  , ("M-S-q"         , kill)
  , ("M-S-m"         , windows swapMaster)
  , ("M-<Backspace>" , nextMatch History (return True))
-- window bringer
  , ("M-a"           , gotoMenuConfig  windowBringerDmenuConfig)
  , ("M-S-a"         , bringMenuConfig windowBringerDmenuConfig)
-- workspace management
  , ("M-w l"         , sendMessage NextLayout)
  , ("M-w p"         , toggleWS' ["NSP"])
  , ("M-w j"         , moveTo Next nonEmptyWS)
  , ("M-w k"         , moveTo Prev nonEmptyWS)
  , ("M-S-w j"       , shiftTo Next nonEmptyWS >> moveTo Next nonEmptyWS)
  , ("M-S-w k"       , shiftTo Prev nonEmptyWS >> moveTo Prev nonEmptyWS)
-- scratchpads
  , ("M-s t"         , namedScratchpadAction scratchpads "terminal")
  , ("M-s b"         , namedScratchpadAction scratchpads "browser")
  , ("M-s h"         , namedScratchpadAction scratchpads "zeal")
  , ("M-s m"         , namedScratchpadAction scratchpads "telegram")
-- floating placement
  , ("M-f f"         , withFocused $ placeFloating floatRectFull)
  , ("M-f c"         , withFocused $ placeFloating floatRectCenter)
  , ("M-f j"         , withFocused $ placeFloating floatRectBottom)
  , ("M-f k"         , withFocused $ placeFloating floatRectTop)
  , ("M-f h"         , withFocused $ placeFloating floatRectLeft)
  , ("M-f l"         , withFocused $ placeFloating floatRectRight)
-- system control
  , ("M-c <Up>"      , spawn "amixer sset Master 10%+")
  , ("M-c <Down>"    , spawn "amixer sset Master 10%-")
  , ("M-c m"         , spawn "amixer sset Master toggle") ]
  `additionalKeys`
  [ ((noModMask, xK_Menu) , namedScratchpadAction scratchpads "terminal") ]

nonEmptyWS = WSIs $ return (\w -> nonNSP w && nonEmpty w)
  where nonNSP (Workspace tag _ _) = tag /= "NSP"
        nonEmpty = isJust . stack

placeFloating :: RationalRect -> Window -> X ()
placeFloating rect = windows . (flip XMonad.StackSet.float $ rect)
