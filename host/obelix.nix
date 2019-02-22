{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  custom.hidpi = false;

  custom.tasks = {
    "compustream_shell" = {
      description = "compustream dev shell";
      directory = "~/projects/dev/compustream";
      terminal = true;
      command = "nix-shell --command fish";
    };
  };
}
