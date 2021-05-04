{ pkgs, ... }:

{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;

  accounts.email = {
    maildirBasePath = "mail";
    
    accounts.automatix = {
      primary = true;
      
      realName = "Adrian Kummerländer";
      address  = "adrian@kummerlaender.eu";

      userName = "adrian@kummerlaender.eu";
      passwordCommand = "${pkgs.pass}/bin/pass it/automatix_mail";

      gpg = {
        key = "61F4C67D12636E70AFB10C3D83E758150AB49859";
      };

      imap = {
        host = "mx.kummerlaender.eu";
        tls.enable = true;
      };

      mbsync = {
        enable = true;
        expunge = "both";
        create = "maildir";
      };

      smtp = {
        host = "mx.kummerlaender.eu";
        port = 587;
        tls.useStartTls = true;
      };

      msmtp.enable = true;
    };

    accounts.KIT = {
      realName = "Adrian Kummerländer";
      address = "adrian.kummerlaender@kit.edu";

      userName = "sb2380@kit.edu";
      passwordCommand = "${pkgs.pass}/bin/pass kit/sb2380";

      imap = {
        host = "imap.kit.edu";
        tls.enable = true;
      };

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        extraConfig.account = {
          AuthMechs = "PLAIN";
        };
      };

      smtp = {
        host = "smtp.kit.edu";
        port = 587;
        tls.useStartTls = true;
      };

      msmtp.enable = true;
    };
  };

  services.mbsync = {
    enable = true;
  };

  home.file.".emacs.d/email.el".source = ./conf/email.el;
}
