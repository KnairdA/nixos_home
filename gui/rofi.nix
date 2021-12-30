{ config, pkgs, ... }:

let
  hidpiExtraConfig = if config.custom.hidpi then {
    dpi = 160;
  } else { };

in {
  programs.rofi = {
    enable = true;
    location = "top";
    font = "Iosevka 12";

    theme = let
      inherit (config.lib.formats.rasi) mkLiteral;
    in {
      " @import" = "default";
      "*" = {
        background = mkLiteral "#222222";
        foreground = mkLiteral "#F2F2F2";
        lightbg = mkLiteral "#161616";
        lightfg = mkLiteral "#F2F2F2";
        blue = mkLiteral "#aadb0f";
      };
      window = {
        border   =  6;
        y-offset = -6;
        border-color = mkLiteral "#aadb0f";
      };
    };

    extraConfig = {
      modi       = "combi";
      combi-modi = "windowcd,drun,ssh";
      terminal = "kitty";
      ssh-command = "{terminal} {ssh-client} {host}";
    } // hidpiExtraConfig;
  };
}
