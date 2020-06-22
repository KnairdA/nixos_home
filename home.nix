{ config, pkgs, ... }:

{
  custom.pkgs = import <mypkgs> { };
  custom.nixpkgs-unstable = import <nixpkgs-unstable> { };

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
    keyboard.layout = "de";

    packages = [
      config.custom.pkgs.persistent-nix-shell
    ];
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    lfs.enable = true;

    userName  = "Adrian Kummerlaender";
    userEmail = "adrian@kummerlaender.eu";

    extraConfig.core.editor = "vim";
    extraConfig.merge.tool = "${pkgs.meld}/bin/meld";
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
        hostname = "172.23.20.188";
        user = "urdzx";
      };
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl     = 120;
    enableSshSupport    = true;
    defaultCacheTtlSsh  = 600;
    enableScDaemon      = false;
  };

  programs.fish.shellAliases = {
    ns  = "persistent-nix-shell -s";
    cat = "bat --plain";
  };

  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;
}
