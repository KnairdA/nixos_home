{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/networkmanager.nix
    ../gui/stalonetray.nix
    ../gui/redshift.nix
  ];

  home.packages = with pkgs; [
    acpi
    brightnessctl
    arandr
    blueman
  ];

  custom.hidpi = false;
}
