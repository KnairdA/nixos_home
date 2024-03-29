{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    twmn
    libnotify
  ];

  systemd.user.services.twmnd = {
    Unit = {
      Description = "twmn notification deamon";
      After  = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      Environment = "PATH=%h/.nix-profile/bin";
      ExecStart   = "${pkgs.twmn}/bin/twmnd";
      Restart     = "on-failure";
    };
  };

  home.file.".config/twmn/twmn.conf".text = ''
    [gui]
    background_color=#909737
    font=Iosevka
    font_size=14
    height=20
    foreground_color=#111111
    always_on_top=true
    position=bottom_right
    bounce=true
    bounce_duration=500
    in_animation=6
    in_animation_duration=500
    out_animation=5
    out_animation_duration=500
    max_length=-1
    opacity=100
    [main]
    duration=5000
    host=127.0.0.1
    port=9797
  '';

}
