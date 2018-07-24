{ pkgs, ... }:

{
  home = {
    packages = [ pkgs.kitty ];

    file.".config/kitty/kitty.conf".text = ''
      font_family Iosevka
      font_size   9
      font_size_delta 1
      adjust_line_height 110%

      background #161616
      foreground #F2F2F2
      # black
      color0     #161616
      color8     #000000
      # red
      color1     #8C3346
      color9     #FF0000
      # green
      color2     #AADB0F
      color10    #909636
      # yellow
      color3     #E4E093
      color11    #FFFF00
      # blue
      color4     #352F6A
      color12    #0000FF
      # magenta
      color5     #CE5C00
      color13    #F57900
      # cyan
      color6     #89B6E2
      color14    #46A4FF
      # white
      color7     #F2F2F2
      color15    #FFFFFF
    '';
  };
}
