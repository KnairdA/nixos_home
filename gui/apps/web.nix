{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = [
      pkgs.tdesktop
      pkgs.tigervnc
      pkgs.remmina

      pkgs-unstable.zotero
    ];
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/http"  = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  programs = {
    firefox = {
      enable = true;
      package = pkgs.firefox.override {
        nativeMessagingHosts = with pkgs; [ passff-host ];
      };
    };
    chromium.enable = true;
  };

  services.syncthing.enable = true;
}
