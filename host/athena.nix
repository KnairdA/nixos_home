{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/networkmanager.nix
    ../gui/hidpi.nix
    ../gui/stalonetray.nix
    ../gui/redshift.nix
    ../gui/touchegg.nix
  ];

  home.packages = with pkgs; [
    acpi
    xorg.xbacklight
  ];
}
