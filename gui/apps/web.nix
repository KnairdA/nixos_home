{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = with pkgs-unstable; [
      tdesktop

      zotero

      teams
      zoom-us

      pkgs.tigervnc
      pkgs.remmina
    ];
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http"  = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  programs = {
    firefox.enable = true;
    chromium.enable = true;
  };

  services.syncthing.enable = true;
}
