{ config, pkgs, ... }:

{
  programs.zathura = {
    enable = true;

    options = {
      font = "Iosevka 16px";

      inputbar-fg   = "#161616";
      inputbar-bg   = "#909737";

      statusbar-fg  = "#161616";
      statusbar-bg  = "#909737";

      completion-fg = "#161616";
      completion-bg = "#909737";

      completion-highlight-fg = "#909737";
      completion-highlight-bg = "#161616";

      recolor-lightcolor      = "#161616";
      recolor-darkcolor       = "#ffffff";
    };
  };
}
