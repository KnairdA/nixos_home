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

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad.hs;
    };

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

  home.packages = with pkgs; [
  # lockscreen
    i3lock
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
