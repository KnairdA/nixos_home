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
    rev = "35eb565c6d00f3c61ef5e74e7e41870cfa3926f7";
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
      description = "Generic LaTeX shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "latex-env" (with pkgs; [
        tectonic
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
