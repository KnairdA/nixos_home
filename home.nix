{ pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
    path = ''https://github.com/rycee/home-manager/archive/master.tar.gz'';
  };

  imports = [
  # define options custom to this config
    ./custom.nix
  # load host specific stuff
    ./host/current.nix
  # task shortcuts common to all setups
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
