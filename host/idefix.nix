{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  home.packages = with pkgs; [
    gnome.gnome-tweaks
  ];
}
