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
    rev = "";
  }) {};

  mkJupyterEnv = kernel: (jupyter.jupyterlabWith {
    kernels = [ kernel ];
  }).env;

in {
  custom.tasks = {
    bsc_edit = {
      description = "Grid refinement BSc thesis editor";
      directory = "~/university/documents/bachelor/arbeit";
      type = "local-editor";
    };

    bsc_shell = {
      description = "Grid refinement BSc thesis shell";
      directory = "~/university/documents/bachelor/arbeit";
      type = "local-shell";
    };

    bsc_view = {
      description = "Grid refinement BSc thesis PDF";
      directory = "~/university/documents/bachelor/arbeit";
      command = "evince build/main.pdf";
    };

    olb_edit = {
      description = "OpenLB editor";
      directory = "~/projects/contrib/openlb";
      type = "local-editor";
    };

    olb_shell = {
      description = "OpenLB shell";
      directory = "~/projects/contrib/openlb";
      type = "local-shell";
    };

    study_edit = {
      description = "University notes editor";
      directory = "~/university/note";
      type = "local-editor";
    };

    study_shell = {
      description = "University notes shell";
      directory = "~/university/note";
      type = "local-shell";
    };

    cpp_shell = {
      description = "Generic C++ shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "cpp-env" (with pkgs; [
        cmake
        gcc8
        gdb cgdb
        universal-ctags
      ]);
    };

    pymath_shell = {
      description = "Python console for mathematics";
      directory = "~/";
      type = "python-console";
      environment = mkPythonShellDerivation "pymath-env" (with pkgs.python3Packages; [
        sympy
        numpy
        matplotlib
      ])
      ''
        import numpy as np
        import sympy
        import matplotlib
        import matplotlib.pyplot as plt

        sympy.init_session()
      '';
    };

    pymath_jupyter = {
      description = "Python @ Jupyter Lab";
      directory = "~/";
      type = "jupyter-lab";
      environment = mkJupyterEnv (
        jupyter.kernels.iPythonWith {
          name = "python";
          packages = p: with p; [
            numpy
            sympy
            matplotlib
          ];
        }
      );
    };
  };
}
