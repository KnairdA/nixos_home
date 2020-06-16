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
        ${pkgs.emacs}/bin/emacs -batch -f batch-byte-compile $out/share/emacs/site-lisp/*.el
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

    extraPackages = epkgs: with epkgs.melpaPackages; [
      pdf-tools
    ] ++ [
      akr-color-theme
      custom-runtime-env
    ];
  };

  home.packages = with pkgs; [
    source-sans-pro
    source-serif-pro
    emacs-all-the-icons-fonts
  ];

  # see https://github.com/rycee/home-manager/issues/589#issuecomment-466594137
  home.activation.linkInitEl = config.lib.dag.entryAfter ["writeBoundary"] ''
    mkdir -p ~/.emacs.d
    ln -s ${toString ./conf/init.el} ~/.emacs.d/init.el
  '';
}
