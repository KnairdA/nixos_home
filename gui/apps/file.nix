{ pkgs, ... }:

{
  home = {
    packages = let
      custom-sxiv = import ./sxiv.nix pkgs;
    in with pkgs; [
    # browser
      pcmanfm
      nnn file
      xfce.gigolo
    # automounting
      gvfs lxmenu-data shared_mime_info
    # tools
      veracrypt
    # viewers
      evince
      custom-sxiv
      mpv
    # office
      libreoffice
      gimp
    ];

    sessionVariables = {
      # required to enable auto-mounting in pcmanfm
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
      # use GTK theme in libreoffice
      SAL_USE_VCLPLUGIN = "gtk";
      # NNN: display folders in bright green
      NNN_CONTEXT_COLORS = "2222";
      # NNN: open all text files in $EDITOR
      NNN_USE_EDITOR = 1;
    };

    file.".config/user-dirs.dirs".text = ''
      XDG_TEMPLATES_DIR="$HOME/"
      XDG_DESKTOP_DIR="$HOME/"
      XDG_DOWNLOADS_DIR="$HOME/downloads/"
    '';
  };
}
