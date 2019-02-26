# dotfiles 2.0

This repository fully describes my current NixOS user environment in a completely reproducible manner using [home-manager](https://github.com/rycee/home-manager/).
Coupling this with the declarative system configuration in [nixos_system](https://code.kummerlaender.eu/nixos_system) I am able to trivially maintain a common computing environment across multiple machines.
Furthermore it is possible to reproduce this environment on a new computer within half an hour given a sufficiently good network connection.

## Overview

The desktop environment consisting of XMonad, custom theming and a selection of utilities and applications is configured by the derivations in the `gui` folder. e.g. check out `gui/xmonad.nix` and `gui/gtk.nix` to get started.

Parts of this setup are maintained in my own Nix channel [pkgs.kummerlaender.eu](https://pkgs.kummerlaender.eu).
e.g. I can build my custom Vim setup on any system that is able to run the Nix package manager (such as a Surface running WSL) using a single command: 

```
nix-build "https://pkgs.kummerlaender.eu/nixexprs.tar.bz2" -A custom-vim
./result/bin/vim
```

## Host specific config

The `host` folder contains a selection of host specific definitions to be selected by symlinking the desired configuration as `host/current.nix`.

## Modules

Recently I started to develop a kind of _module_ to generate application shortcuts that are then easily launchable via Rofi. This is very useful for quickly starting shells and editors in project-specific `nix-shell` environments or maintaining generic playground environments for various programming languages. See `tasks/default.nix` and `module/tasker.nix` for details.
