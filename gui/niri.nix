{ config, pkgs, ... }:

{
  imports = [
    ./gtk.nix
    ./dunst.nix
  ];

  home.pointerCursor = {
    enable = true;
    package = pkgs.vanilla-dmz;
    name    = "Vanilla-DMZ-AA";
    size    = 16;
  };

  #home.file.".config/niri/config.kdl".source = config.lib.file.mkOutOfStoreSymlink ./conf/niri.kdl;

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.swaylock}/bin/swaylock -c #000000";
  };
}
