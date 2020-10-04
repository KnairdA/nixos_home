{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = (with pkgs; [
      tdesktop
    ]) ++ (with pkgs-unstable; [
      zotero
    ]);
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http"  = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  programs.firefox.enable = true;

  services.syncthing.enable = true;
}
