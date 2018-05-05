{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
    # browser
      pcmanfm
    # automounting
      gvfs lxmenu-data shared_mime_info
    # tools
      veracrypt
    # viewers
      evince
      sxiv
      mpv
      libreoffice
    ];

    sessionVariables = {
      # required to enable auto-mounting in pcmanfm
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
      # use GTK theme in libreoffice
      SAL_USE_VCLPLUGIN = "gtk";
    };

    file.".config/user-dirs.dirs".text = ''
      XDG_TEMPLATES_DIR="$HOME/"
      XDG_DESKTOP_DIR="$HOME/"
      XDG_DOWNLOADS_DIR="$HOME/downloads/"
    '';
  };
}
