{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    location = "top";
    font = "Iosevka 12";
    separator = "none";
    scrollbar = false;
    borderWidth =  6;
    yoffset     = -6;

    colors = {
      window = {
        background = "#222222";
        border     = "#aadb0f";
        separator  = "#000000";
      };
      rows = {
        normal = {
          background = "#222222";
          foreground = "#8e8e8e";
          backgroundAlt = "#161616";
          highlight = {
            background = "#111111";
            foreground = "#aadb0f";
          };
        };
        active = {
          background = "#222222";
          foreground = "#8e8e8e";
          backgroundAlt = "#161616";
          highlight = {
            background = "#111111";
            foreground = "#aadb0f";
          };
        };
        urgent = {
          background = "#222222";
          foreground = "#dc322f";
          backgroundAlt = "#161616";
          highlight = {
            background = "#dc322f";
            foreground = "#161616";
          };
        };
      };
    };

    extraConfig = ''
      rofi.modi: combi
      rofi.combi-modi: windowcd,drun,ssh
      rofi.terminal: kitty
      rofi.ssh-command: {terminal} {ssh-client} {host}
      rofi.dpi: 160
    '';
  };
}
