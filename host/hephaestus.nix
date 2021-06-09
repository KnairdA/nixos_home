{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  custom.hidpi = false;

  services.kdeconnect = {
    enable = true;
  };
}
