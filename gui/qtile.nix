{ config, pkgs, ... }:

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

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name    = "Vanilla-DMZ-AA";
      size    = if hidpi then 48 else 16;
    };

    windowManager.command = "qtile";
  };

  home.packages = with pkgs; [
  # wm
    qtile
  # lockscreen
    i3lock
  ];

  home.file.".config/qtile/config.py".source = ./conf/qtile.py;

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
  };

  services.flameshot.enable = true;
}
