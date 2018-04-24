import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Util.Themes

import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS

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

main = xmonad $ ewmh defaultConfig
  { modMask             = mod4Mask -- super key as modifier
  , borderWidth         = 3
  , normalBorderColor   = "#161616"
  , focusedBorderColor  = "#909737"
  , terminal            = "urxvt"
  , handleEventHook     = handleEventHook defaultConfig <+> fullscreenEventHook
  , layoutHook          = availableLayouts
  }
  `removeKeysP`
  [ "M-S-<Return>" ]
  `additionalKeysP`
-- application launchers
  [ ("M-p"           , spawn "rofi -show combi")
  , ("M-<Return>"    , spawn "urxvt")
  , ("M-<Backspace>" , spawn "vim -g")
-- actual window management
  , ("M-<Left>"      , prevWS)
  , ("M-<Right>"     , nextWS)
  , ("M-S-<Left>"    , shiftToPrev >> prevWS)
  , ("M-S-<Right>"   , shiftToNext >> nextWS)
  , ("M-S-m"         , windows swapMaster)
-- system control
  , ("M-s <Up>"      , spawn "amixer sset Master 10%+")
  , ("M-s <Down>"    , spawn "amixer sset Master 10%-")
  , ("M-s m"         , spawn "amixer sset Master toggle")
  ]
