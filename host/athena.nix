{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/networkmanager.nix
    ../gui/stalonetray.nix
    ../gui/redshift.nix
    ../gui/touchegg.nix
  ];

  home.packages = with pkgs; [
    acpi
    xorg.xbacklight
  ];

  xresources.extraConfig = ''
    Xft.dpi: 160
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintfull
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';
}
