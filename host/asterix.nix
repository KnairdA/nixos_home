{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/lowdpi.nix
    ../gui/autorandr.nix
    ../gui/networkmanager.nix
  ];

  home.packages = with pkgs; [
    acpi
  ];
}
