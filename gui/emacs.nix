{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    extraPackages = (epkgs: (with epkgs.melpaStablePackages; [ 
      use-package
      leuven-theme
    ]) ++ (with epkgs.melpaPackages; [ 
      evil
      evil-leader
      evil-org
    ]) ++ (with epkgs.elpaPackages; [ 
      org
    ]));
  };

  home.file.".emacs.d/init.el".source = ./conf/init.el;
}
