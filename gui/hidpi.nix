{ pkgs, ... }:

{
  home.sessionVariables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };

  xresources.extraConfig = ''
    Xft.dpi: 160
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintfull
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';

  home.file.".config/twmn/twmn.conf".text = ''
    font_size=24
    height=32
  '';

  programs.rofi.extraConfig = ''
    rofi.dpi: 160
  '';
}
