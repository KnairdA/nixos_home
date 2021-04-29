{ config, pkgs, sources, ... }:

let
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [ (import sources.emacs-overlay) ];
  };

in {
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
                               ":${tex}/bin:${pkgs.pandoc}/bin"))
        (add-to-list 'exec-path "${tex}/bin")
        (add-to-list 'exec-path "${pkgs.graphviz}/bin")
        (add-to-list 'exec-path "${pkgs.sqlite}/bin")
        (add-to-list 'exec-path "${pkgs.universal-ctags}/bin")
        (add-to-list 'exec-path "${pkgs.global}/bin")
        (add-to-list 'exec-path "${pkgs.pandoc}/bin")
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

    package = pkgs-unstable.emacsWithPackagesFromUsePackage {
      config = ./conf/init.el;
      alwaysEnsure = false;
      # remove builtin org as in https://github.com/chrisbarrett/.emacs.d/blob/6efd82c8e328e677dbef84331ed54763b89667a3/default.nix
      # this is a workaround until I find a better way to force usage of a non-builtin up-to-date org version
      package = pkgs-unstable.emacsGit.override { nativeComp = true; };
      extraEmacsPackages = epkgs: (with epkgs.melpaPackages; [
        pdf-tools
        mu4e-alert
      ]) ++ (with epkgs.elpaPackages; [
        auctex
      ])++ [
        akr-color-theme
        custom-runtime-env
        pkgs.mu
      ];
    };
  };

  home.packages = let
    org-protocol-shortcut = pkgs.writeTextFile {
      name        = "org-protocol";
      executable  = false;
      destination = "/share/applications/org-protocol.desktop";
      text = ''
        [Desktop Entry]
        Type=Application
        Name=Emacs (Client, Protocol)
        Exec=emacsclient %u
        Terminal=false
        MimeType=x-scheme-handler/org-protocol
      '';
    };

  in with pkgs; [
    source-sans-pro
    source-serif-pro
    emacs-all-the-icons-fonts
    mu
    org-protocol-shortcut
  ];

  home.file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink ./conf/init.el;

  systemd.user.services.emacs.Service.ExecStart = pkgs.lib.mkForce "${pkgs.runtimeShell} -l -c 'exec emacs --fg-daemon'";

  services.emacs = {
    enable = true;
  };
}
