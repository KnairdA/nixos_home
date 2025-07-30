{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/niri.nix
    ../gui/networkmanager.nix
  ];

  home.packages = with pkgs; [
    acpi
    brightnessctl
    blueman
    xwayland-satellite
  ];

  home.sessionVariables = {
    QT_QPA_PLATFORM = "wayland";
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
  };
}
