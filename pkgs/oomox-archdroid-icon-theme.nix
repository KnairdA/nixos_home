{ stdenv, pkgs, color, ... }:

stdenv.mkDerivation rec {
  name = "oomox-archdroid-icon-theme";

  src = pkgs.fetchFromGitHub {
    repo   = "archdroid-icon-theme";
    owner  = "themix-project";
    rev    = "6dc4c92689ea2ce99534d6f461283efdf8ffd270";
    sha256 = "073iwaamzrmj0l6h4gzmbnmazq16lgpc027mr3l778b35qwwraq5";
  };

  postPatch = ''
    patchShebangs .
  '';

  installPhase = ''
    HOME="$out/share/icons"
    sed -i "66s/\.icons\///" change_color.sh
    ./change_color.sh --output oomox-archdroid --color ${color}
  '';

  meta = {
    description = "oomox-archdroid-icon-theme is a customizable fork of archdroid-icon-theme";
    homepage = https://github.com/themix-project/archdroid-icon-theme;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
