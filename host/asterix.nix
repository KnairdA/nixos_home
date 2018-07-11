{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  home.packages = let
  # only asterix has a 3g modem
    modem-manager-gui = import ../gui/pkgs/modem-manager-gui.nix pkgs;
  in [
    modem-manager-gui
  ];
}
