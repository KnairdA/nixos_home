{ pkgs, ... }:

{
  home = {
    packages = [ pkgs.kitty ];

    file.".config/kitty/kitty.conf".text = ''
      font_family iosevka
      font_size   10
      font_size_delta 1
      adjust_line_height 110%

      background #161616
      foreground #F2F2F2
      # black
      color0     #161616
      color8     #F2F2F2
      # red
      color1     #8C3346
      color9     #ff0000
      # green
      color2     #aadb0f
      color10    #909636
      # yellow
      color3     #E4E093
      color11    #ffff00
      # blue
      color4     #352F6A
      color12    #0000ff
      # magenta
      color5     #ce5c00
      color13    #f57900
      # cyan
      color6     #89b6e2
      color14    #46a4ff
      # white
      color7     #F2F2F2
      color15    #ffffff
    '';
  };
}
