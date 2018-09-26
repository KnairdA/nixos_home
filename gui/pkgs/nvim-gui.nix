{ pkgs, ... }:

let
  mypkgs = import (fetchTarball "https://pkgs.kummerlaender.eu/nixexprs.tar.gz") { };
in pkgs.neovim-qt.override {
  neovim = mypkgs.custom-neovim;
}
