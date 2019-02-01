{ pkgs, ... }:

{
  xsession.pointerCursor.size = 16;

  home.file.".config/twmn/twmn.conf".text = ''
    [gui]
    font_size=15
    height=20
  '';
}
