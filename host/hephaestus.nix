{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/niri.nix
    ../gui/networkmanager.nix
  ];

  services = {
    kdeconnect.enable = true;
    screen-locker.enable = pkgs.lib.mkForce false;
  };
}
