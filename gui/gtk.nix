{ pkgs, ... }:

{
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

    iconTheme = let
      oomox-archdroid-icon-theme = pkgs.callPackage ./pkgs/oomox-archdroid-icon-theme.nix {
        color = "909737";
      };
    in {
      name    = "oomox-archdroid";
      package = oomox-archdroid-icon-theme;
    };

    font = {
      name    = "Iosevka 10";
      package = pkgs.iosevka;
    };

    gtk3.extraConfig = {
      gtk-decoration-layout = "";
    };
  };

  qt = {
    enable      = true;
    useGtkTheme = true;
  };
}
