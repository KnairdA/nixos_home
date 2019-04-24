{ config, pkgs, ... }:

let
  hidpi = config.custom.hidpi;

in {
  programs.zathura = {
    enable = true;

    options = {
      font = if hidpi then "Iosevka 28px" else "Iosevka 14px";
      
      inputbar-bg   = "#161616";
      inputbar-fg   = "#909737";

      statusbar-bg  = "#161616";
      statusbar-fg  = "#909737";

      completion-bg = "#161616";
      completion-fg = "#909737";

      completion-highlight-bg = "#909737";
      completion-highlight-fg = "#161616";

      recolor-lightcolor      = "#161616";
      recolor-darkcolor       = "#ffffff";
    };
  };
}
