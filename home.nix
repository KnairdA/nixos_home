{ pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
    path = ''https://github.com/rycee/home-manager/archive/master.tar.gz'';
  };

  home = {
    keyboard.layout = "de";

    packages = with pkgs; [
      rxvt_unicode
    # UI fonts
      iosevka
    # file viewers
      zathura
      sxiv
      mpv
    # file management
      pcmanfm gvfs lxmenu-data shared_mime_info
      veracrypt
    # communication
      tdesktop
    # UI dev utilities (CLI utilities are added in project specific nix-shells)
      paraview
      hotspot
      qcachegrind
      gitg
    ];

    # required to enable auto-mounting in pcmanfm
    sessionVariables = {
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };

    # vim is configured globally for all users but common is the only GUI user
    file.".gvimrc".text = ''
      set guifont=Iosevka\ 10
      set linespace=2
      set guioptions=agim
      set guioptions-=m
      2match SpecialKeyTab /\t/
    '';
  };

  programs.git = {
    enable = true;
    userName  = "Adrian Kummerlaender";
    userEmail = "knairda@t-online.de";
    extraConfig.core.editor = "vim";
  };

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

  xresources.extraConfig = builtins.readFile ./conf/urxvt.Xresources;

  gtk = {
    enable = true;

    theme = let
      oomox-gtk-theme = pkgs.callPackage ./pkgs/oomox-gtk-theme.nix {
        pkgs-unstable = import <nixpkgs-unstable> {};
        theme = {
          accent_bg  = "aadb0f";
          bg         = "d8d8d8";
          fg         = "101010";
          btn_bg     = "f5f5f5";
          btn_fg     = "111111";
          caret_size = 0.04;
          caret1_fg  = "101010";
          caret2_fg  = "101010";
          hdr_btn_bg = "161616";
          hdr_btn_fg = "aadb0f";
          menu_bg    = "909737";
          menu_fg    = "1a1a1a";
          sel_bg     = "aadb0f";
          sel_fg     = "101010";
          txt_bg     = "ffffff";
          txt_fg     = "101010";
          gradient   = 0.0;
          roundness  = 0;
          spacing    = 1;
          wm_border_focus   = "909737";
          wm_border_unfocus = "909737";
          gtk3_generate_dark = false;
        };
      };
    in {
      name    = "oomox";
      package = oomox-gtk-theme;
    };

    gtk2.extraConfig = ''
      style "vimfix" { bg[NORMAL] = "#161616" }
      widget "vim-main-window.*GtkForm" style "vimfix"
    '';
  };

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

  home.file.".config/zathura/zathurarc".text = builtins.readFile ./conf/pwmt/zathurarc;
}
