{ pkgs, ... }:

let
  tasks = {
    "bsc_edit" = {
      description = "Grid refinement BSc thesis editor";
      directory = "~/university/documents/bachelor/arbeit";
      command = "nix-shell --run 'nvim-qt --no-ext-tabline'";
    };
    "bsc_view" = {
      description = "Grid refinement BSc thesis PDF";
      directory = "~/university/documents/bachelor/arbeit";
      command = "evince build/main.pdf";
    };
  };
in {
  home.packages = let
    task_derivations = pkgs.callPackage ./pkgs/tasker.nix { inherit pkgs; inherit tasks; };
  in task_derivations;
}
