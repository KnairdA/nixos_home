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
      freecad
    # audio
      pulseaudio
      pavucontrol
    # office
      libreoffice
      gimp
      simplescreenrecorder
    ];

    sessionVariables = {
    # required to enable auto-mounting in pcmanfm
      GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
    # use GTK theme in libreoffice
      SAL_USE_VCLPLUGIN = "gtk";
    };

    file.".config/user-dirs.dirs".text = ''
      XDG_TEMPLATES_DIR="$HOME/"
      XDG_DESKTOP_DIR="$HOME/"
      XDG_DOWNLOAD_DIR="$HOME/downloads/"
      XDG_DOCUMENTS_DIR="$HOME/"
      XDG_MUSIC_DIR="$HOME/"
      XDG_PICTURES_DIR="$HOME/"
      XDG_PUBLICSHARE_DIR="$HOME/"
      XDG_TEMPLATES_DIR="$HOME/"
      XDG_VIDEOS_DIR="$HOME/"
    '';
    file.".config/user-dirs.dirs".force = true;
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
