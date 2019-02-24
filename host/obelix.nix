{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  custom.hidpi = false;

  custom.tasks = {
    "compustream_shell" = {
      description = "compustream dev shell";
      type = "terminal";
      directory = "~/projects/dev/compustream";
      command = "nix-shell --command fish";
    };
  };
}
