{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/autorandr.nix
    ../gui/networkmanager.nix
  ];

  home.packages = with pkgs; [
    acpi
  ];

  custom.hidpi = false;
}
