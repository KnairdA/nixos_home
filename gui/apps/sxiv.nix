{ pkgs, ... }:

pkgs.callPackage ../pkgs/sxiv.nix {
  theme = {
    font_name = "Iosevka";
    font_size = "12";
    bg = "#161616";
    fg = "#909636";
  };
}
