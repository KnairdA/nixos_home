{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
    # UI dev utilities (language environments are maintained in project specific nix-shells)
      zeal
      hotspot
      qcachegrind
      gitg
      paraview
    # language utilities
      artha
    ];
  };
}
