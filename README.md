# dotfiles 2.0

This repository fully describes my current NixOS user environment in a completely reproducible manner using [home-manager](https://github.com/rycee/home-manager/).
Coupling this with the declarative system configuration in [nixos_system](https://code.kummerlaender.eu/nixos_system) I am able to trivially maintain a common computing environment across multiple machines.
Furthermore it is possible to reproduce this environment on a new computer in the time it takes to download and build the derivations.

## Overview

The desktop environment consisting of XMonad, custom theming and a selection of utilities and applications is configured by the derivations in the `gui` folder. e.g. check out `gui/xmonad.nix` and `gui/gtk.nix` to get started.

`home-manager` doesn't have to be installed in the user environment and is only required for applying the configuration.

## Host specific config

The `host` folder contains a selection of host specific definitions to be selected via the hostname declared by the global system config.

## Instantiation

If you are me and want to instantiate the setup on a given host:

```sh
nix flake clone git+https://code.kummerlaender.eu/nixos_home --dest dotfiles 
cd dotfiles
nix run .#host
```

## Modules

Recently I started to develop a kind of _module_ to generate application shortcuts that are then easily launchable via Rofi. This is very useful for quickly starting shells and editors in project-specific `nix-shell` environments or maintaining generic playground environments for various programming languages. See `tasks/default.nix` and `module/tasker.nix` for details.
