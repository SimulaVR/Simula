{
  lib,
  stdenv,
  callPackage,
  godot3-export-templates,
  godot3-headless,
}:

stdenv.mkDerivation rec {
  pname = "simula";
  version = "0.0.0-dev";
  src = lib.cleanSource ./.;

  nativeBuildInputs = [
    godot3-headless
  ];

  buildInputs = [
  ];

  buildPhase = ''
    runHook preBuild

    # Cannot create file '/homeless-shelter/.config/godot/projects/...'
    export HOME=$TMPDIR

    # Link the export-templates to the expected location. The --export commands
    # expects the template-file at .../templates/{godot-version}.stable/linux_x11_64_release
    mkdir -p $HOME/.local/share/godot
    ln -s ${godot3-export-templates}/share/godot/templates $HOME/.local/share/godot

    mkdir -p $out/share/simula

    godot3-headless --export "Linux/X11" $out/share/simula/out

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    ln -s $out/share/simula/out $out/bin/simula

    # Patch binaries.
    interpreter=$(cat $NIX_CC/nix-support/dynamic-linker)
    #patchelf \
    #  --set-interpreter $interpreter \
    #  --set-rpath ${lib.makeLibraryPath buildInputs} \
    #  $out/share/test-godot3-with-nix/out

    runHook postInstall
  '';

  meta = {
    platforms = lib.platforms.unix;
  };
}
