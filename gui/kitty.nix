{ pkgs, ... }:

{
  home = {
    packages = [ pkgs.kitty ];

    file.".config/kitty/kitty.conf".text = ''
      allow_remote_control yes

      font_family Iosevka
      font_size   10
      font_size_delta 0.5
      adjust_line_height 110%

      disable_ligatures always

      enable_audio_bell no

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

      map ctrl+shift+plus  change_font_size all +1.0
      map ctrl+shift+minus change_font_size all -1.0
      map ctrl+shift+0     change_font_size all 0
    '';
  };

  programs.fish.shellAliases = {
    icat = "kitty +kitten icat";
  };
}
