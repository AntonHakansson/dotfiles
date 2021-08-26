final: prev: {
  dwm = prev.dwm.overrideAttrs (oldAttrs: rec {
    src = prev.fetchFromGitHub {
      owner = "bakkeby";
      repo = "dwm-flexipatch";
      rev = "c2e4fed9182c84c713b24e6f3c60754c950bcf9b";
      sha256 = "sha256-bBhcqULh/5XJExW5u7tnt6uvOQEV7WarnnseLpsu20c=";
    };
    buildInputs = oldAttrs.buildInputs ++ [ ];
    postPatch = ''
      cp ${./config.h} config.def.h
      cp ${./patches.h} patches.def.h
    '';
    preBuild = ''
      makeFlagsArray+=(XRENDER="-lXrender")                 # BAR_ALPHA_PATCH
      makeFlagsArray+=(XCBLIBS="-lX11-xcb -lxcb -lxcb-res") # SWALLOW_PATCH
    '';
  });

  dwmblocks = prev.dwmblocks.overrideAttrs (oldAttrs: rec {
    patches = oldAttrs.patches ++ [
      (final.fetchpatch {
        url =
          "https://dwm.suckless.org/patches/statuscmd/dwmblocks-statuscmd-20210402-96cbb45.diff";
        sha256 = "sha256-ADFjvViuTOnY4pMYwVypUPW71CzrLUDIzXr/htldEvk=";
      })
    ];
  });
}
