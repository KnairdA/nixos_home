{ pkgs, ... }:

{
  home = {
    # vim is configured globally for all users but common is the only GUI user
    file.".gvimrc".text = ''
      set guifont=Iosevka\ 10
      set linespace=2
      set guioptions=agi
    '';

    # nvim-qt using vim configuration
    packages = let
      nvim-gui = import ./pkgs/nvim-gui.nix pkgs;
    in [
      nvim-gui
      pkgs.xclip # required to access clipboard in nvim-gui
    ];

    file.".config/nvim/ginit.vim".text = ''
      set title
      Guifont! Iosevka:h10

      autocmd FocusGained * :checktime
    '';
  };

  gtk = {
    gtk3.extraCss = ''
      window#vim-main-window {
          background-color: #161616;
      }
    '';
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      nvim-qt = "nvim-qt --no-ext-tabline ^ /dev/null > /dev/null";
    };
  };
}
