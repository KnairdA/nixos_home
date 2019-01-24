{ pkgs, ... }:

{
  home.packages = [ pkgs.networkmanager_dmenu ];

  home.file.".config/networkmanager-dmenu/condig.ini".text = ''
    [dmenu]
    dmenu_command  = rofi
    rofi_highlight = True
    [editor]
    gui_if_available = True
  '';
}
