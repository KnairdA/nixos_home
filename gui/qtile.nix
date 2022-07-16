{ config, pkgs, pkgs-unstable, ... }:

let
  hidpi = config.custom.hidpi;

in {
  imports = [
    ./gtk.nix
    ./rofi.nix
    ./twmn.nix
  ];

  xsession = {
    enable = true;

    windowManager.command = "qtile start";

    initExtra = ''
      xsetroot -solid "#000000"
    '';
  };

  home.pointerCursor = {
    x11.enable = true;
    package = pkgs.vanilla-dmz;
    name    = "Vanilla-DMZ-AA";
    size    = if hidpi then 48 else 16;
  };

  home.packages = [
  # wm
    pkgs-unstable.qtile
  # lockscreen
    pkgs.i3lock
  ];

  home.file.".config/qtile/config.py".source = ./conf/qtile.py;

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
  };

  services.flameshot.enable = true;
}
