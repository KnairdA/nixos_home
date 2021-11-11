{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = with pkgs-unstable; [
      tdesktop

      zotero

      teams
      zoom-us

      tigervnc
      remmina
    ];
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http"  = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  programs.firefox.enable = true;
  programs.chromium.enable = true;

  services.syncthing.enable = true;
}
