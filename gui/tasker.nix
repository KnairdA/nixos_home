{ pkgs, ... }:

let
  tasks = {
    "bsc_edit" = {
      description = "Grid refinement BSc thesis editor";
      directory = "~/university/documents/bachelor/arbeit";
      terminal = false;
      command = "nix-shell --run 'nvim-qt --no-ext-tabline'";
    };
    "bsc_shell" = {
      description = "Grid refinement BSc thesis shell";
      directory = "~/university/documents/bachelor/arbeit";
      terminal = true;
      command = "nix-shell --command fish";
    };
    "bsc_view" = {
      description = "Grid refinement BSc thesis PDF";
      directory = "~/university/documents/bachelor/arbeit";
      terminal = false;
      command = "evince build/main.pdf";
    };
  };
in {
  home.packages = pkgs.callPackage ./pkgs/tasker.nix { inherit pkgs; inherit tasks; };
}
