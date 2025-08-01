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
        notification_limit = 5;
        width = "(0, 800)";
        height = 40;
        offset = "6x6";
        origin = "bottom-right";
        transparency = 10;
        frame_color = "#909737";
        background = "#909737";
        foreground = "#111111";
        font = "Iosevka 10";
      };
    };
  };
}
