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

  programs.firefox.enable = true;

  services.syncthing.enable = true;
}
