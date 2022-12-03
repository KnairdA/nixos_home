{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/xmonad.nix
    ../gui/networkmanager.nix
    ../gui/stalonetray.nix
  ];

  services = {
    kdeconnect.enable = true;
    screen-locker.enable = pkgs.lib.mkForce false;
  };
}
