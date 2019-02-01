{ pkgs, ... }:

{
  xresources.extraConfig = ''
    Xft.dpi: 160
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintfull
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';

  xsession.pointerCursor.size = 48;

  home.file.".config/twmn/twmn.conf".text = ''
    [gui]
    font_size=24
    height=32
  '';

  programs.rofi.extraConfig = ''
    rofi.dpi: 160
  '';
}
