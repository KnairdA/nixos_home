{ pkgs, ... }:

{
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
      size    = 16;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad.hs;
    };
  };

  home.packages = with pkgs; [
  # lockscreen
    i3lock
  # screenshooter
    xfce4-12.xfce4-screenshooter
  ];

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
  };
}
