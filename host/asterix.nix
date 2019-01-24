{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/autorandr.nix
    ../gui/networkmanager-dmenu.nix
  ];

  home.packages = with pkgs; [
    acpi
  ];
}
