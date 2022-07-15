{ pkgs, ... }:

{
  home = {
    packages = let
      custom-sxiv = pkgs.callPackage ../pkgs/sxiv.nix { };
    in with pkgs; [
      pcmanfm
    # automounting
      gvfs lxmenu-data shared-mime-info
      xfce.gigolo
    # viewers
      bat
      evince
      custom-sxiv
      mpv
      gthumb
    # audio
      pulseaudio
      pavucontrol
      pulseeffects-pw
    # office
      libreoffice
      gimp
      simplescreenrecorder
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
      XDG_DOWNLOAD_DIR="$HOME/downloads/"
    '';
  };

  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "org.gnome.Evince.desktop" ];

    "image/png"  = [ "sxiv.desktop" ];
    "image/jpg"  = [ "sxiv.desktop" ];
    "image/jpeg" = [ "sxiv.desktop" ];

    "video/mp4" = [ "mpv.desktop" ];
  };

# sxiv config
  xresources.extraConfig = ''
    Sxiv.font: Iosevka:size=12
    Sxiv.foreground: #909636
    Sxiv.background: #161616
  '';
}
