{ config, pkgs, ... }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  pkgs-unstable = import sources.nixpkgs-unstable { };
  pkgs-personal = import sources.mypkgs { };

in {
  _module.args.sources = sources;
  _module.args.pkgs-unstable = pkgs-unstable;
  _module.args.pkgs-personal = pkgs-personal;

  imports = [
  # define options custom to this config
    ./custom.nix
  # load host specific stuff
    ./host/current.nix
  # task shortcuts
    ./module/tasker.nix
    ./tasks/default.nix
  ];

  home = {
    keyboard = {
      layout = "de";
      options = [ "caps:escape" ];
    };

    packages = [
      pkgs-personal.persistent-nix-shell
    ];
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    lfs.enable = true;

    userName  = "Adrian Kummerlaender";
    userEmail = "adrian@kummerlaender.eu";

    extraConfig = {
      core.editor = "vim";
      merge.tool = "${pkgs.meld}/bin/meld";
      pull.ff = "only";
      init.defaultBranch = "master";
    };
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "automatix" = {
        hostname = "kummerlaender.eu";
        user = "common";
      };
      "majestix" = {
        hostname = "10.100.0.3";
        user = "common";
      };
      "horst" = {
        proxyCommand = "ssh majestix -W %h:%p";
        hostname = "MVMITS177.mvm.kit.edu";
        user = "urdzx";
      };
      "bwunicluster" = {
        proxyCommand = "ssh majestix -W %h:%p";
        hostname = "bwunicluster.scc.kit.edu";
        user = "urdzx";
      };
    };
  };

  systemd.user.startServices = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport    = true;
    enableScDaemon      = false;

    defaultCacheTtl    = 43200;
    maxCacheTtl        = 43200;
    defaultCacheTtlSsh = 43200;
    maxCacheTtlSsh     = 43200;
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      ns  = "persistent-nix-shell -s";
      cat = "bat --plain";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  xdg.mimeApps.enable = true;
}
