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
      command = "nix-shell --run 'nvim-qt --no-ext-tabline'";
    };
    bsc_shell = {
      description = "Grid refinement BSc thesis shell";
      type = "terminal";
      directory = "~/university/documents/bachelor/arbeit";
      command = "nix-shell --command fish";
    };
    bsc_view = {
      description = "Grid refinement BSc thesis PDF";
      directory = "~/university/documents/bachelor/arbeit";
      command = "evince build/main.pdf";
    };
    olb_edit = {
      description = "OpenLB editor";
      directory = "~/projects/contrib/openlb";
      command = "nix-shell --run 'nvim-qt --no-ext-tabline'";
    };
    olb_shell = {
      description = "OpenLB shell";
      type = "terminal";
      directory = "~/projects/contrib/openlb";
      command = "nix-shell --command fish";
    };
    cpp_shell = {
      description = "Generic C++ shell environment";
      type = "environment";
      directory = "~/";
      environment = mkShellDerivation "cpp-env" (with pkgs; [
        cmake
        gcc8
        gdb cgdb
      ]);
    };
  };
}
