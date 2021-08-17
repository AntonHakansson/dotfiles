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
      makeFlagsArray+=(XRENDER="-lXrender") # BAR_ALPHA_PATCH
      makeFlagsArray+=(XCBLIBS="-lX11-xcb -lxcb -lxcb-res") # SWALLOW_PATCH
    '';
  });

  dwmblocks = prev.dwmblocks.overrideAttrs (oldAttrs: rec {
    src = prev.fetchFromGitHub {
      owner = "LukeSmithxyz";
      repo = "dwmblocks";
      rev = "66f31c307adbdcc2505239260ecda24a49eea7af";
      sha256 = "sha256-j3wCRyl1+0D2XcdqhE5Zgf53bEXhcaU4dvdyYG9LZ2g=";
    };
    buildInputs = oldAttrs.buildInputs
      ++ [ prev.xorg.libXft prev.xorg.libXinerama ];
    prePatch = ''
      sed -i "s@/usr/local/@$out@" Makefile
    '';
    preBuild = ''
      mv blocks.def.h config.h
    '';
  });
}
