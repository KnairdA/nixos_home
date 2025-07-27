{ config, pkgs, ... }:

{
  imports = [
    ./gtk.nix
  ];

  home.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name    = "Vanilla-DMZ-AA";
    size    = 16;
  };

  #home.file.".config/niri/config.kdl".source = config.lib.file.mkOutOfStoreSymlink ./conf/niri.kdl;

  home.packages = with pkgs; [
    swaylock
  ];

  services.screen-locker = {
    enable  = true;
    lockCmd = "${pkgs.swaylock}/bin/swaylock -c #000000";
  };
}
