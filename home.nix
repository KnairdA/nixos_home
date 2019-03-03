{ config, pkgs, ... }:

{
  custom.pkgs = import <mypkgs> {
    pkgs = import <nixpkgs-unstable> { };
  };

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
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userName  = "Adrian Kummerlaender";
    userEmail = "adrian@kummerlaender.eu";
    extraConfig.core.editor = "vim";
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "automatix" = {
        hostname = "kummerlaender.eu";
        user = "common";
      };
      "horst" = {
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
}
