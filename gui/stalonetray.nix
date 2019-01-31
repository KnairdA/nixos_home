{ pkgs, ... }:

{
  services.stalonetray = {
    enable = true;

    config = {
      decorations  = "none";
      icon_size    = 48;
      geometry     = "5x1+10-10";
      transparent  = false;
      grow_gravity = "E";
      background   = "#000000";
      dockapp_mode = "none";
      window_layer = "bottom";
      window_strut = "bottom";
      window_type  = "dock";
    };
  };
}
