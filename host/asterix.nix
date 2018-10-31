{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/autorandr.nix
  ];

  home.packages = let
    mypkgs = import (fetchTarball "https://pkgs.kummerlaender.eu/nixexprs.tar.gz") { };
  in [
  # only asterix has a 3g modem
    mypkgs.modem-manager-gui
    pkgs.networkmanager_dmenu
  ];
}
