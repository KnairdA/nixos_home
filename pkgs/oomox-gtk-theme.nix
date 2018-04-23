with import <nixpkgs-unstable> {};

stdenv.mkDerivation rec {
  name = "oomox-gtk-theme";

  src = fetchFromGitHub {
    repo   = "oomox-gtk-theme";
    owner  = "themix-project";
    rev    = "aa9081b2899d7e8ba8ae47543173d2d9f0f13921";
    sha256 = "1q4nksnkhdpfpgcbfqbmnkjrmwxa6zv3wy43zlas2agssjkcm4x9";
  };

  nativeBuildInputs = [ sass sassc librsvg glib libxml2 gdk_pixbuf bc ];
  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  config = ''
    ACCENT_BG=aadb0f
    BG=d8d8d8
    FG=101010
    BTN_BG=f5f5f5
    BTN_FG=111111
    CARET1_FG=101010
    CARET2_FG=101010
    CARET_SIZE=0.04
    GRADIENT=0.0
    GTK3_GENERATE_DARK=False
    HDR_BTN_BG=161616
    HDR_BTN_FG=aadb0f
    MATERIA_STYLE_COMPACT=True
    MENU_BG=909737
    MENU_FG=1a1a1a
    SEL_BG=aadb0f
    SEL_FG=101010
    TXT_BG=ffffff
    TXT_FG=101010
    WM_BORDER_FOCUS=909737
    WM_BORDER_UNFOCUS=909737
    ROUNDNESS=0
    SPACING=1
  '';

  installPhase = ''
    cd oomox-gtk-theme-bb6169b10b12e8e9672ba828fa503d885b5041d5
    HOME="$out/share/themes/oomox"
    ./change_color.sh -m all --target-dir "$out/share/themes" --output oomox <(echo -e "${config}")
  '';

  meta = {
    description = "oomox-gtk-theme is a customizable fork of Numix-gtk-theme.";
    homepage = https://github.com/themix-project/oomox-gtk-theme;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
