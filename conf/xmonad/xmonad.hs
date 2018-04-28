import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet

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

scratchpads = [ NS "terminal" "kitty --class=scratchterm" (className =? "scratchterm")
                   (customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3))
              , NS "telegram" "telegram-desktop" (className =? "TelegramDesktop")
                   (customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3)) ]

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
  [ "M-S-<Return>", "M-q", "M-S-q", "M-S-c" ]
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
  , ("M-w j"         , nextWS)
  , ("M-w k"         , prevWS)
  , ("M-S-w j"       , shiftToNext >> nextWS)
  , ("M-S-w k"       , shiftToPrev >> prevWS)
-- scratchpads
  , ("M-s t"         , namedScratchpadAction scratchpads "terminal")
  , ("M-s m"         , namedScratchpadAction scratchpads "telegram")
-- system control
  , ("M-c <Up>"      , spawn "amixer sset Master 10%+")
  , ("M-c <Down>"    , spawn "amixer sset Master 10%-")
  , ("M-c m"         , spawn "amixer sset Master toggle") ]
  `additionalKeys`
  [ ((noModMask, xK_Menu) , namedScratchpadAction scratchpads "terminal") ]
