{ pkgs, ... }:

{
  services.redshift = {
    enable = true;
    tray = true;
    latitude  = "49.0135248";
    longitude = "8.40435918703854";
  };
}
