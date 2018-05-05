{ pkgs, ... }:

{
  imports = [
    ./xmonad.nix
    ./terminal.nix
    ./vim.nix
  # applications grouped by purpose
    ./apps/file.nix
    ./apps/web.nix
    ./apps/dev.nix
  ];
}
