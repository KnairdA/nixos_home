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
      passwordCommand = "pass it/automatix_mail";

      gpg = {
        key = "61F4C67D12636E70AFB10C3D83E758150AB49859";
      };

      imap = {
        host = "mx.kummerlaender.eu";
        tls.enable = true;
      };

      mbsync = {
        enable = true;
        create = "maildir";
      };

      smtp = {
        host = "mx.kummerlaender.eu";
        port = 587;
        tls.useStartTls = true;
      };

      msmtp.enable = true;
    };

    accounts.kit = {
      realName = "Adrian Kummerländer";
      address = "adrian.kummerlaender@student.kit.edu";

      userName = "urdzx@student.kit.edu";
      passwordCommand = "pass kit";

      imap = {
        host = "imap.kit.edu";
        tls.enable = true;
      };

      mbsync = {
        enable = true;
        create = "maildir";
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
}
