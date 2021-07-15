{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/networkmanager.nix
    ../gui/stalonetray.nix
  ];

  custom.hidpi = false;

  services.kdeconnect = {
    enable = true;
  };
}
