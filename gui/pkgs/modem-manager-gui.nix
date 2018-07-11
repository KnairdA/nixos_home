{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
  name = "modem-manager-gui";

  src = pkgs.fetchhg {
    url = "https://linuxonly@bitbucket.org/linuxonly/modem-manager-gui";
    rev = "5c6456f";
  };

  # required for locale generation
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  # required to get Po4a to work
  PERL5LIB="${pkgs.perlPackages.Po4a}/lib/perl5";

  buildInputs = with pkgs; [
    pkgconfig
    gtk3
    glib
    gdbm
    perlPackages.Po4a
    itstool
    libappindicator-gtk3
    meson
    gettext
    ninja
  ];

  meta = {
    description = "modem-manager-gui";
    homepage = https://linuxonly.ru/page/modem-manager-gui;
    license = stdenv.lib.licenses.gpl3;
  };
}
