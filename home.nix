{ pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
    path = ''https://github.com/rycee/home-manager/archive/master.tar.gz'';
  };

  imports = [
    ./host/current.nix
  ];

  home = {
    keyboard.layout = "de";
  };

  programs.git = {
    enable = true;
    userName  = "Adrian Kummerlaender";
    userEmail = "knairda@t-online.de";
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
