self: super: {
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    src = ./dwm-6.2;
  });
}
