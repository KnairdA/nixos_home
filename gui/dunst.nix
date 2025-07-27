{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    swaylock
    libnotify
  ];

  services.dunst = {
    enable = true;
    settings = {
      global = {
        width = 300;
        height = 50;
        offset = "5x5";
        origin = "bottom-right";
        transparency = 10;
        frame_color = "#909737";
        background = "#909737";
        foreground = "#111111";
        font = "Iosevka 11";
      };
    };
  };
}
