{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      pcmanfm gvfs lxmenu-data shared_mime_info
      veracrypt
    ];

    sessionVariables = {
      # required to enable auto-mounting in pcmanfm
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
      # use GTK theme in libreoffice
      SAL_USE_VCLPLUGIN = "gtk";
    };
  };
}
