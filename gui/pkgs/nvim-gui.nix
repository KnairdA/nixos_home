{ config, pkgs, ... }:

pkgs.neovim-qt.override {
  neovim = config.custom.pkgs.custom-neovim;
}
