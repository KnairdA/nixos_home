{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
    # file viewers
      sxiv
      mpv
      paraview
      libreoffice
    # communication
      thunderbird
      tdesktop
    # UI dev utilities (CLI utilities are added in project specific nix-shells)
      zeal
      hotspot
      qcachegrind
      gitg
    ];

    file.".config/user-dirs.dirs".text = ''
      XDG_TEMPLATES_DIR="$HOME/"
      XDG_DESKTOP_DIR="$HOME/"
      XDG_DOWNLOADS_DIR="$HOME/downloads/"
    '';
  };

  imports = [
  # desktop environment
    ./xmonad.nix
    ./rofi.nix
    ./gtk.nix
  # terminals
    ./kitty.nix
    ./urxvt.nix
  # tools
    ./vim.nix
    ./pcmanfm.nix
  # file viewers
    ./zathura.nix
  ];

  services.syncthing.enable = true;

  programs.firefox = {
    enable = true;
    enableAdobeFlash = true;
  };
}
