{ pkgs, ... }:

{
  home.packages = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet
  ];

  services.network-manager-applet.enable = true;

  home.file.".config/networkmanager-dmenu/config.ini".text = ''
    [dmenu]
    dmenu_command  = rofi
    rofi_highlight = True
    [editor]
    gui_if_available = True
  '';
}
