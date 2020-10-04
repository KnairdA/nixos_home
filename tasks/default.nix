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
      jupyterlab
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

  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "7a6716f0c0a5538691a2f71a9f12b066bce7d55c";
  }) {};

  mkJupyterEnv = kernel: jupyter.jupyterlabWith {
    kernels = [ kernel ];
  };

in {
  custom.tasks = {
    cpp_shell = {
      description = "Generic C++ shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "cpp-env" (with pkgs; [
        cmake
        gcc10
        gdb cgdb
        universal-ctags
      ]);
    };

    latex_shell = {
      description = "Complete TeX Live shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "latex-env" (with pkgs; [
        texlive.combined.scheme-full
      ]);
    };

    pymath_jupyter = {
      description = "Python for mathematics @ Jupyter Lab";
      directory = "~/";
      type = "jupyter-lab";
      environment = mkJupyterEnv (
        jupyter.kernels.iPythonWith {
          name = "python";
          packages = p: with p; [
            numpy
            sympy
            scipy
            matplotlib
          ];
        }
      );
    };
  };
}
