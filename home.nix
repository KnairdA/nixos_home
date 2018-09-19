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
        port = 222;
        user = "common";
      };
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl     = 120;
    enableSshSupport    = true;
    defaultCacheTtlSsh  = 120;
    enableScDaemon      = false;
  };
}
