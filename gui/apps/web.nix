{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      thunderbird
      tdesktop
    ];
  };

  programs.firefox.enable = true;

  services.syncthing.enable = true;
}
