{
  description = "Computing environment of Adrian Kummerlaender";

  inputs = {
    stable.url = github:NixOS/nixpkgs/nixos-23.11;
    unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    oldstable.url = github:NixOS/nixpkgs/nixos-22.11;
    personal.url = github:KnairdA/pkgs;
    home-manager = {
      url = github:nix-community/home-manager/release-23.11;
      inputs = { nixpkgs.follows = "stable"; };
    };
    emacs.url = github:nix-community/emacs-overlay/master;
    jupyter.url = github:GTrunSec/jupyterWith/flakes;
  };

  outputs = {
    self, stable, unstable, oldstable, personal, emacs, home-manager, jupyter, ...
  }: let
    system = "x86_64-linux";

    jupyter-overlay = (final: prev: {
      jupyterWith = jupyter.defaultPackage."${final.system}";
    });

    pkgs = import stable {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [ jupyter-overlay emacs.overlay ];
    };

    pkgs-unstable = import unstable {
      inherit system;
      config = { allowUnfree = true; };
    };

    pkgs-oldstable = import oldstable {
      inherit system;
      config = { allowUnfree = true; };
    };

    pkgs-personal = personal;

    config = hostname: home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      extraSpecialArgs = {
        inherit pkgs-unstable;
        inherit pkgs-oldstable;
        inherit pkgs-personal;
        inherit hostname;
      };

      modules = [ ./home.nix ];
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
