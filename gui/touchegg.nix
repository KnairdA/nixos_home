{ pkgs, ... }:

{
  home.packages = [ pkgs.touchegg ];

  home.file.".config/touchegg/touchegg.conf".source = ./conf/touchegg.conf;

  systemd.user.services.touchegg = {
    Unit = {
      Description = "Touchégg multitouch gestures";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.touchegg}/bin/touchegg";
      Restart = "on-failure";
    };
  };
}
