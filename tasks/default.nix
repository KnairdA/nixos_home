{ pkgs, ... }:

let
  mkShellDerivation = n: ps: pkgs.stdenvNoCC.mkDerivation rec {
    name = n;
    buildInputs = ps;
    shellHook = ''
      export NIX_SHELL_NAME="${name}"
    '';
  };

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

    cpp_shell = {
      description = "Generic C++ shell environment";
      directory = "~/";
      type = "environment";
      environment = mkShellDerivation "cpp-env" (with pkgs; [
        cmake
        gcc8
        gdb cgdb
      ]);
    };
  };
}
