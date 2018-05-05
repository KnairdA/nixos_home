{ pkgs, ... }:

{
  home = {
    # vim is configured globally for all users but common is the only GUI user
    file.".gvimrc".text = ''
      set guifont=Iosevka\ 10
      set linespace=2
      set guioptions=agim
      set guioptions-=m
      2match SpecialKeyTab /\t/
    '';
  };

  gtk = {
    gtk2.extraConfig = ''
      style "vimfix" { bg[NORMAL] = "#161616" }
      widget "vim-main-window.*GtkForm" style "vimfix"
    '';
  };
}
