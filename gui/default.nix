{ config, pkgs, ... }:

{
  imports = [
    ./fish.nix
    ./kitty.nix
    ./emacs.nix
    ./email.nix
    ./zathura.nix
    ./htop.nix
    ./pass.nix
    ./rofi.nix

  # applications grouped by purpose
    ./apps/file.nix
    ./apps/web.nix
    ./apps/dev.nix
  ];

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    iosevka
  ];

  home.sessionVariables = {
    QT_AUTO_SCREEN_SCALE_FACTOR = 0;
  };

}
