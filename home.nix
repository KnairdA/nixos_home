{ pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
    path = ''https://github.com/rycee/home-manager/archive/master.tar.gz'';
  };

  home.packages = with pkgs; [
    rxvt_unicode
    pcmanfm
    zathura
    sxiv
    mpv
    iosevka
    tdesktop
    veracrypt
  ];

  programs.git = {
    enable = true;
    userName  = "Adrian Kummerlaender";
    userEmail = "knairda@t-online.de";
    extraConfig.core.editor = "vim";
  };

  home.keyboard.layout = "de";

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl     = 120;
    enableSshSupport    = true;
    defaultCacheTtlSsh  = 120;
    enableScDaemon      = false;
  };

  xsession = {
    enable = true;
    initExtra = ''
      xsetroot -cursor_name left_ptr
      xset b off
    '';

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad/xmonad.hs;
    };
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

  home.file.".gvimrc".text = ''
    set guifont=Iosevka\ 10
    set linespace=2
    set guioptions=agim
    set guioptions-=m
    2match SpecialKeyTab /\t/
  '';

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
    '';
  };

  programs.firefox = {
    enable = true;
    enableAdobeFlash = true;
  };
}
