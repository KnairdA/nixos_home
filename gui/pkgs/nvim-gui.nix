{ pkgs, ... }:

let
  custom_nvim = pkgs.neovim.override {
    vimAlias  = false;
    configure = (import /etc/nixos/pkgs/vim/custom.nix { pkgs = pkgs; });
  };
in pkgs.neovim-qt.override {
  neovim = custom_nvim;
}
