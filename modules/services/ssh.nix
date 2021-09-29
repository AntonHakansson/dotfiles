{ options, config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
    };

    user.openssh.authorizedKeys.keys = if config.user.name == "hakanssn" then
      [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDHO6/JVINeragZOgo2xSNuMuIUED44XohdX5wlEKQvxnVYi9okTzrit+1YWAnRafOsyEhndEgRequLqWXVJVUMGRWVAxVwKb/nQ608Y86eHyV0vGcz+T8BpsEjUDCHTjKYJrhQcyeUWRWk1HYRluB1CgdfbEbRdnCdJoGgokA4qCNZmqDNbAST5+VQMMP8yNKt45uYmbq5dFWVABKTNjf3BdyXHet1vKdCQfcy2BRQJciMIaaPuDiJBGWKZavwBMPD3/woqvPEXEpdlWL1d9LNm7t/cQpp0eVSFJC7Rca4sNo8suJDiJ2RxYHT5ZvU+Tm/meFvsUWARDsoHmZRWdA8h/ynOWUFdGy9B724UpUVXQ6YnFR6BFpv3oRiTpnaQRTVUwBTizDJFWssSsnad4uWWkUezsH+RrgihaqyhEawuapD8KkhuGueOSMol+4juddEBcMZs9UVfU5o2wTupgNxh/lNK6Y5HPPuerUAbxcOZ8D/Dtg5HAS1xeBHSJLDT8yMl4PqFJ3NRggP7SBMLe+wXa75DXH7JyuiIJRbbM2+cFmULAJ6nkFUKTDgLH8dnVx9d1Tkr5UQ/+31v1r/i5HgeYSKyPjMWCGwdI9mZUKnz41gUp9x9iaH7BGSop8oMi12lvUUg8k1jF3kZNWDE7Nwe5SnBtwOzx0ngEPbFHnYDw== anton.hakansson98@gmail.com"
      ]
    else
      [ ];
  };
}
