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

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad.hs;
    };
  };

  home.packages = let
    tasker = pkgs.lib.callPackageWith pkgs ./pkgs/tasker.nix { };
  in with pkgs; [
  # lockscreen
    i3lock
  # task launcher
    tasker
  ];

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
  };

  programs.fish.shellAliases = {
    mph = "mpv --title hud";
  };

  services.flameshot.enable = true;
}
