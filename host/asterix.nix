{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/autorandr.nix
  ];

  home.packages = let
    mypkgs = import (fetchTarball "https://pkgs.kummerlaender.eu/nixexprs.tar.gz") { };
  in [
    pkgs.networkmanager_dmenu
    pkgs.acpi
  ];
}
