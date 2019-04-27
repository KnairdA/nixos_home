{ config, pkgs, ... }:

let
  hidpi = config.custom.hidpi;
in {
  gtk = {
    enable = true;

    theme = {
      name = "oomox";
      package = config.custom.pkgs.oomox-gtk-theme {
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
        menu_bg    = "909636";
        menu_fg    = "1a1a1a";
        sel_bg     = "aadb0f";
        sel_fg     = "101010";
        txt_bg     = "ffffff";
        txt_fg     = "101010";
        gradient   = 0.0;
        roundness  = 0;
        spacing    = if hidpi then 5 else 1;
        wm_border_focus   = "909636";
        wm_border_unfocus = "909636";
        gtk3_generate_dark = false;
        gtk2_hidpi = false;
      };
    };

    iconTheme = {
      name    = "oomox-archdroid";
      package = config.custom.pkgs.oomox-archdroid-icon-theme "909636";
    };

    font = {
      name    = "Iosevka 10";
      package = pkgs.iosevka;
    };

    gtk3 = {
      extraConfig = {
        gtk-decoration-layout = "";
      };
      extraCss = ''
        menubar, .menubar {
          -GtkWidget-window-dragging: false;
        }
      '';
    };
  };

  home.packages = [
  # enable persistent settings in e.g. file chooser dialogs
    pkgs.gnome3.dconf
    pkgs.gnome3.dconf-editor
  ];

  qt = {
    enable = true;
    platformTheme = "gtk";
  };
}
