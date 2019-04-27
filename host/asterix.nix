{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/networkmanager.nix
    ../gui/stalonetray.nix
    ../gui/autorandr.nix
  ];

  home.packages = with pkgs; [
    acpi
  ];

  custom.hidpi = false;
}
