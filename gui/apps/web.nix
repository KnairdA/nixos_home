{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = with pkgs-unstable; [
      zotero
      teams
      zoom-us
      tdesktop
      tigervnc
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
