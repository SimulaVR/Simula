{ stdenv, fetchFromGitHub, haskellPackages, callPackage, buildEnv, xrdb, wmctrl, SDL2, lib, onNixOS ? false, xwayland, xkbcomp, ghc, ffmpeg-full, midori, xfce, devBuild, fontconfig, glibcLocales, dejavu_fonts, writeScriptBin, coreutils, curl, vulkan-loader, ulauncher }:
let
    vulkan-loader-custom = if onNixOS then vulkan-loader else (callPackage ./nix/vulkan-loader.nix { });
    glibc-locales = glibcLocales;
    godot = callPackage ./submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; };
    godot-api = "${godot}/bin/api.json";
    godot-haskell = haskellPackages.callPackage ./submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };
    godot-haskell-plugin = haskellPackages.callPackage ./addons/godot-haskell-plugin/godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; godot = godot; godot-haskell = godot-haskell; };

    ghc-version = ghc.version;

    xpra = callPackage ./nix/xpra/default.nix { };

    devBuildFalse = ''
      cp GetNixGL.sh $out/bin/GetNixGL.sh
      ln -s ${godot}/bin/godot.x11.opt.debug.64 $out/bin/godot.x11.opt.debug.64
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64
      ln -s ${godot}/bin/godot.x11.opt.64 $out/bin/godot.x11.opt.64
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula
      echo "if [ ! -d .import ]; then LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.tools.64 --export \"Linux/X11\" ./result/bin/SimulaExport; fi" >> $out/bin/simula
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.opt.debug.64 -m;" >> $out/bin/simula
      echo "sed -in \"s/\$USER/anon/g\" output.file" >> $out/bin/simula
      echo "${curl}/bin/curl --data-urlencode errorMsg@output.file https://www.wolframcloud.com/obj/george.w.singer/errorMessage" >> $out/bin/simula
      chmod +x $out/bin/simula

     '';

    devBuildTrue = ''
      cp GetNixGL.sh $out/bin/GetNixGL.sh
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64

      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula
      echo "if [ ! -d .import ]; then PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.tools.64 -e; else PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.tools.64 -m 2>&1 | ${coreutils}/bin/tee output.file; fi" >> $out/bin/simula

      echo "${curl}/bin/curl --data-urlencode errorMsg@output.file https://www.wolframcloud.com/obj/george.w.singer/errorMessage" >> $out/bin/simula
      chmod +x $out/bin/simula

     '';

    devBuildScript = if (devBuild == true) then devBuildTrue else devBuildFalse;

    # Ensure xfce4-terminal has working fonts on every distribution
    xfce4-terminal-wrapped = writeScriptBin "xfce4-terminal" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${xfce.xfce4-terminal}/bin/xfce4-terminal
      '';

    simula = stdenv.mkDerivation {
      name = "Simula";
      src = ./utils;
      buildInputs = [ godot godot-haskell-plugin xpra xrdb wmctrl fontconfig glibc-locales xfce4-terminal-wrapped ];
      installPhase = ''
      mkdir -p $out/bin
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/libgodot-haskell-plugin.so $out/bin/libgodot-haskell-plugin.so
      ln -s ${xpra}/bin/xpra $out/bin/xpra
      ln -s ${xfce4-terminal-wrapped}/bin/xfce4-terminal $out/bin/xfce4-terminal
      ln -s ${xrdb}/bin/xrdb $out/bin/xrdb
      ln -s ${wmctrl}/bin/wmctrl $out/bin/wmctrl
      ln -s ${ffmpeg-full}/bin/ffplay $out/bin/ffplay
      ln -s ${midori}/bin/midori $out/bin/midori
      ln -s ${ulauncher}/bin/ulauncher $out/bin/ulauncher

      '' + devBuildScript;
    };

in simula
