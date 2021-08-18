{
  description = "Computing environment of Adrian Kummerlaender";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.05;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    home-manager = {
      url = github:nix-community/home-manager/release-21.05;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    emacs.url = github:nix-community/emacs-overlay/master;
    personal = {
      url = https://pkgs.kummerlaender.eu/nixexprs.tar.xz;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, emacs, home-manager, personal, ... }: let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };

  in {
    homeManagerConfigurations = {
      common = home-manager.lib.homeManagerConfiguration {
        configuration = { pkgs, ... }: {
          _module.args = {
            pkgs-personal = import personal { };
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
              config = { allowUnfree = true; };
              overlays = [ emacs.overlay ];
            };
          };
          imports = [ ./home.nix ];
          nixpkgs = {
            config = { allowUnfree = true; };
          };
        };
        system = system;
        homeDirectory = "/home/common";
        username = "common";
      };
    };
  };
}
