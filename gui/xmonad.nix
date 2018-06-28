{ pkgs, ... }:

{
  imports = [
    ./gtk.nix
    ./rofi.nix
  ];

  xsession = {
    enable = true;

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name    = "Vanilla-DMZ-AA";
      size    = 16;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad.hs;
    };
  };

  home.packages = [
  # lockscreen
    pkgs.i3lock
  ];
}
