{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
    # UI dev utilities (language environments are maintained in project specific nix-shells)
      zeal
      hotspot
      qcachegrind
      paraview
    # version control
      gitg
      meld
    # language utilities
      artha
    # calculator
      speedcrunch
    # ctag to override the emacs provided ones (placeholder)
      universal-ctags
      fzf
    ];
  };
}
