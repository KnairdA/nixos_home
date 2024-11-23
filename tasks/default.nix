{ pkgs, ... }:

let
  mkShellDerivation = n: ps: pkgs.stdenvNoCC.mkDerivation rec {
    name = n;
    buildInputs = ps;
    shellHook = ''
      export NIX_SHELL_NAME="${name}"
    '';
  };

  mkPythonShellDerivation = n: ps: init: pkgs.stdenvNoCC.mkDerivation rec {
    name = n;
    buildInputs = [(pkgs.python3.withPackages (python-packages: with python-packages; ps ++ [
      qtconsole
    ]))];
    shellHook = let
      startup = pkgs.writeTextFile {
        name = "startup.py";
        text = init;
      };

    in ''
      export NIX_SHELL_NAME="${name}"
      export PYTHONSTARTUP=${startup}
    '';
  };

in {
  custom.tasks = {
    cpp_shell = {
      description = "Generic C++ shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "cpp-env" (with pkgs; [
        cmake
        gcc13
        gdb cgdb
        universal-ctags
      ]);
    };

    java_shell = {
      description = "Generic Java shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "java-env" (with pkgs; [
        gnumake
        openjdk
      ]);
    };

    latex_shell = {
      description = "Generic LaTeX shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "latex-env" (with pkgs; [
        texlive.combined.scheme-full
        biber
      ]);
    };
  };
}
