{ lib, stdenv, makeWrapper, libnotify, curl, gnugrep, rofi, transmission }:

stdenv.mkDerivation {
  pname = "notflix";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cp notflix $out/bin/notflix
    wrapProgram $out/bin/notflix \
      --prefix PATH : ${
        lib.makeBinPath [ libnotify curl gnugrep rofi transmission ]
      }
  '';
}
