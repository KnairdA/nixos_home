{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
    # UI dev utilities (language environments are maintained in project specific nix-shells)
      zeal
      hotspot
      kcachegrind
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

  home.file.".ctags.d/exclude.ctags".text = ''
    --exclude=*.html
    --exclude=.git
    --exclude=.gcroots
  '';
}
