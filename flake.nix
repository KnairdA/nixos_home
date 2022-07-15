{
  description = "Computing environment of Adrian Kummerlaender";

  inputs = {
    stable.url = github:NixOS/nixpkgs/nixos-22.05;
    unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    personal.url = github:KnairdA/pkgs;
    home-manager = {
      url = github:nix-community/home-manager/release-22.05;
      inputs = { nixpkgs.follows = "stable"; };
    };
    emacs.url = github:nix-community/emacs-overlay/master;
    jupyter.url = github:GTrunSec/jupyterWith/flakes;
  };

  outputs = {
    self, stable, unstable, personal, emacs, home-manager, jupyter, ...
  }: let
    system = "x86_64-linux";

    pkgs-unstable = import unstable {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [ emacs.overlay ];
    };

    pkgs-personal = personal;

    jupyter-overlay = (final: prev: {
      jupyterWith = jupyter.defaultPackage."${final.system}";
    });

    config = hostname: home-manager.lib.homeManagerConfiguration {
      system = system;
      homeDirectory = "/home/common";
      username = "common";
      extraSpecialArgs = {
        inherit pkgs-unstable;
        inherit pkgs-personal;
        inherit hostname;
      };
      configuration = { ... }: {
        imports = [ ./home.nix ];
        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [ jupyter-overlay ];
        };
      };
    };

    hostnames = builtins.map
      (h: builtins.replaceStrings [ ".nix" ] [ "" ] h)
      (stable.lib.mapAttrsToList (name: type: name) (builtins.readDir ./host));

  in {
    packages.x86_64-linux = builtins.listToAttrs
      (map (h: { name = h; value = (config h).activationPackage; }) hostnames);

    apps.x86_64-linux = builtins.listToAttrs
      (map (h: { name = h; value = {
                   type = "app";
                   program = "${(config h).activationPackage}/activate";
                 };
               }) hostnames);
  };
}
