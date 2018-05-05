{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      thunderbird
      tdesktop
    ];
  };

  programs.firefox = {
    enable = true;
    enableAdobeFlash = true;
  };

  services.syncthing.enable = true;
}
