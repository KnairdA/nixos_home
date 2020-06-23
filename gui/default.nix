{ config, pkgs, ... }:

let
  hidpi = config.custom.hidpi;

in {
  imports = [
    ./xmonad.nix
    ./kitty.nix
    ./vim.nix
    ./emacs.nix
    ./email.nix
    ./zathura.nix
    ./pass.nix

  # applications grouped by purpose
    ./apps/file.nix
    ./apps/web.nix
    ./apps/dev.nix
  ];

  fonts.fontconfig.enable = true;

# hidpi specific xorg flags
  xresources.extraConfig = pkgs.lib.mkIf hidpi ''
    Xft.dpi: 160
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintfull
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';
}
