{ config, pkgs, ... }:

{
  home = {
    packages = let
      neovim-qt = pkgs.neovim-qt.override {
        neovim = config.custom.pkgs.custom-neovim;
      };
    in [
      neovim-qt
      pkgs.xclip # required to access clipboard in nvim-gui
      pkgs.fzf   # required for all kinds of narrow-down-completion
    ];

    file.".config/nvim/ginit.vim".text = ''
      set title
      Guifont! Iosevka:h10
      set guifont=Iosevka:h10

      autocmd FocusGained * :checktime
    '';
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      nvim-qt = "nvim-qt --no-ext-tabline ^ /dev/null > /dev/null";
    };
  };
}
