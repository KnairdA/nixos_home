{ config, pkgs, pkgs-personal, hostname, ... }:

{
  imports = [
  # define options custom to this config
    ./custom.nix
  # load host specific stuff
    (./host + ("/" + hostname + ".nix"))
  # task shortcuts
    ./module/tasker.nix
    ./tasks/default.nix
  ];

  home = {
    stateVersion = "22.11";

    username = "common";
    homeDirectory = "/home/common";

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
      gitlab.user = "KnairdA";
    };
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "automatix" = {
        hostname = "kummerlaender.eu";
        user = "common";
      };
      "atlas" = {
        hostname = "10.100.0.3";
        user = "common";
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
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  xdg = {
    mimeApps.enable = true;
    configFile."mimeapps.list".force = true;
  };
}
