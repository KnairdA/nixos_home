{ pkgs ? import <nixpkgs> { }, ... }:

{
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
  };
}
