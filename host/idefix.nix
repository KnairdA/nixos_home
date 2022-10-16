{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  home.packages = with pkgs; [
    gnome3.gnome-tweaks
  ];
}
