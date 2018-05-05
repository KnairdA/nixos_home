{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      kitty
      rxvt_unicode
    ];

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

  xresources.extraConfig = ''
    URxvt.saveLines:             10000
    URxvt.scrollBar:             false
    URxvt.font:                  xft:Iosevka:pixelsize=12
    URxvt.letterSpace:           0
    URxvt.transparent:           false
    URxvt.inheritPixmap:         false
    URxvt.fading:                20
    URxvt.dynamicColors:         on

    URxvt.perl-ext-common:       default,matcher,clipboard,resize-font
    URxvt.matcher.button:        1
    URxvt.matcher.pattern.1:     \\bwww\\.[\\w-]\\.[\\w./?&@#-]*[\\w/-]
    URxvt.url-launcher:          firefox

    URxvt.keysym.Control-Up:     \033[1;5A
    URxvt.keysym.Control-Down:   \033[1;5B
    URxvt.keysym.Control-Left:   \033[1;5D
    URxvt.keysym.Control-Right:  \033[1;5C

    URxvt.keysym.M-Down:         \033[1;3B
    URxvt.keysym.M-Up:           \033[1;3A
    URxvt.keysym.M-Left:         \033[1;3D
    URxvt.keysym.M-Right:        \033[1;3C

    URxvt.keysym.M-c:            perl:clipboard:copy
    URxvt.keysym.M-v:            perl:clipboard:paste

    URxvt.background:            #161616
    URxvt.foreground:            #F2F2F2

    ! black
    URxvt.color0:                #161616
    URxvt.color8:                #F2F2F2
    ! red
    URxvt.color1:                #8C3346
    URxvt.color9:                #ff0000
    ! green
    URxvt.color2:                #aadb0f
    URxvt.color10:               #909636
    ! yellow
    URxvt.color3:                #E4E093
    URxvt.color11:               #ffff00
    ! blue
    URxvt.color4:                #352F6A
    URxvt.color12:               #0000ff
    ! magenta
    URxvt.color5:                #ce5c00
    URxvt.color13:               #f57900
    ! cyan
    URxvt.color6:                #89b6e2
    URxvt.color14:               #46a4ff
    ! white
    URxvt.color7:                #F2F2F2
    URxvt.color15:               #ffffff
  '';
}
