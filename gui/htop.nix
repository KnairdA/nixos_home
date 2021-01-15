{ config, pkgs, ... }:

{
  programs.htop = {
    enable = true;

    treeView = true;
    showProgramPath = false;
    highlightBaseName = true;
    hideThreads = true;
    hideUserlandThreads = true;
  };
}
