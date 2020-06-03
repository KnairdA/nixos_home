{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    extraPackages = (epkgs: (with epkgs.melpaStablePackages; [ 
    ]) ++ (with epkgs.melpaPackages; [ 
      pdf-tools
    ]) ++ (with epkgs.elpaPackages; [ 
    ]));
  };

  home.packages = with pkgs; [
    source-sans-pro
    source-serif-pro
  ];

  # see https://github.com/rycee/home-manager/issues/589#issuecomment-466594137
  home.activation.linkInitEl = config.lib.dag.entryAfter ["writeBoundary"] ''
    mkdir -p ~/.emacs.d
    ln -s ${toString ./conf/init.el} ~/.emacs.d/init.el
  '';
}
