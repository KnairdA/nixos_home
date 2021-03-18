{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = (with pkgs; [
      tdesktop
      tigervnc
    ]) ++ (with pkgs-unstable; [
      zotero
      teams
    ]);
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http"  = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  programs.firefox.enable = true;
  programs.chromium.enable = true;

  services.syncthing.enable = true;
}
