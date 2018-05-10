{ pkgs, ... }:

pkgs.callPackage ../pkgs/sxiv.nix {
  theme = {
    bar_font_name = "Iosevka";
    bar_font_size = "12";
    win_bg = "#000000";
    win_fs = "#999999";
    sel    = "#aadb0f";
    bar_bg = "#161616";
    bar_fg = "#909737";
  };
}
