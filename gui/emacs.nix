{ config, pkgs, ... }:

{
  programs.emacs = let
    akr-color-theme = pkgs.stdenv.mkDerivation {
      name = "emacs-color-theme-akr";
      src = ./conf/metakr.org;
      phases = [ "installPhase" ];
      installPhase = ''
        cp $src metakr.org
        ${pkgs.emacs}/bin/emacs --batch --eval "(require 'org)" --eval "(setq org-confirm-babel-evaluate nil)" --eval '(org-babel-tangle-file "metakr.org")'
        rm metakr.org
        mkdir -p $out/share/emacs/site-lisp
        mv akr-theme.el $out/share/emacs/site-lisp/
      '';
    };

    custom-runtime-env-el = let
      tex = pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-small dvipng;
      };
    in pkgs.writeTextFile {
      name = "custom-runtime-env.el";
      text = ''
        (setenv "PATH" (concat (getenv "PATH")
                               ":${tex}/bin"))
        (add-to-list 'exec-path "${tex}/bin")
        (add-to-list 'exec-path "${pkgs.graphviz}/bin")
        (add-to-list 'exec-path "${pkgs.sqlite}/bin")

        (setq counsel-etags-update-tags-backend (lambda (src-dir) (shell-command "${pkgs.universal-ctags}/bin/ctags -e -R")))
      '';
    };

    custom-runtime-env = pkgs.stdenv.mkDerivation {
      name = "emacs-custom-runtime-env";
      phases = [ "installPhase" ];

      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${custom-runtime-env-el} $out/share/emacs/site-lisp/custom-runtime-env.el
      '';
    };
  in {
    enable = true;

    package = let
      pkgs-unstable = import <nixpkgs-unstable> {
        overlays = [
          (import (builtins.fetchTarball {
            url = https://github.com/nix-community/emacs-overlay/archive/c10d59874dfa8341237702bce514a763198770c4.tar.gz;
          }))
        ];
      };
    in
      pkgs-unstable.emacsWithPackagesFromUsePackage {
        config = ./conf/init.el;
        package = pkgs-unstable.emacsGcc;
        extraEmacsPackages = epkgs: with epkgs.melpaPackages; [
          pdf-tools
          mu4e-alert
        ] ++ [
          akr-color-theme
          custom-runtime-env
          pkgs.mu
        ];
      };
  };

  home.packages = with pkgs; [
    source-sans-pro
    source-serif-pro
    emacs-all-the-icons-fonts
    mu
  ];

  # see https://github.com/rycee/home-manager/issues/589#issuecomment-466594137
  home.activation.linkInitEl = config.lib.dag.entryAfter ["writeBoundary"] ''
    mkdir -p ~/.emacs.d
    ln -s ${toString ./conf/init.el} ~/.emacs.d/init.el
  '';

  systemd.user.services.emacs.Service.ExecStart = pkgs.lib.mkForce "${pkgs.runtimeShell} -l -c 'exec emacs --fg-daemon'";

  services.emacs = {
    enable = true;
  };
}
