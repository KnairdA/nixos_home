{ config, pkgs, ... }:

{
  home = {
    packages = (with pkgs; [
      thunderbird
      tdesktop
    ]) ++ (with config.custom.nixpkgs-unstable; [
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
