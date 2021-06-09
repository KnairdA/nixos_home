{ config, pkgs, ... }:

{
  programs.htop = {
    enable = true;

    settings = {
      tree_view = true;

      show_program_path = false;
      highlight_base_name = true;

      hide_threads         = true;
      hide_userland_threads = true;

      left_meter_modes = [ "LeftCPUs4" "Memory" "Swap" ];
      right_meter_modes = [ "RightCPUs4" "Tasks" "LoadAverage" "Uptime" ];
    };
  };
}
