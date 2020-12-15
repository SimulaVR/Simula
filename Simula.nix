{ stdenv, fetchFromGitHub, haskellPackages, callPackage, buildEnv, xrdb, wmctrl, SDL2, lib, onNixOS ? false, xwayland, xkbcomp, ghc, ffmpeg-full, midori, xfce, devBuild, fontconfig, glibcLocales, dejavu_fonts, writeScriptBin, coreutils, curl, vulkan-loader, ulauncher, mimic, xsel, xclip, dialog }:
let
    vulkan-loader-custom = if onNixOS then vulkan-loader else (callPackage ./nix/vulkan-loader.nix { });
    glibc-locales = glibcLocales;
    godot = callPackage ./submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; };
    godot-api = "${godot}/bin/api.json";
    godot-haskell = haskellPackages.callPackage ./submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };
    godot-haskell-plugin = haskellPackages.callPackage ./addons/godot-haskell-plugin/godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; godot = godot; godot-haskell = godot-haskell; };

    ghc-version = ghc.version;

    xpra = callPackage ./nix/xpra/default.nix { };
    openxr-loader = callPackage ./nix/openxr-loader/default.nix { };
    rr = callPackage ./nix/rr/unstable.nix {};

    devBuildFalse = ''
      cp GetNixGL.sh $out/bin/GetNixGL.sh
      ln -s ${godot}/bin/godot.x11.opt.debug.64 $out/bin/godot.x11.opt.debug.64
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64
      ln -s ${godot}/bin/godot.x11.opt.64 $out/bin/godot.x11.opt.64
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula
      echo "if [ ! -d .import ]; then LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.tools.64 --export \"Linux/X11\" ./result/bin/SimulaExport; fi" >> $out/bin/simula
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.opt.debug.64 -m 2>&1" >> $out/bin/simula
      echo "sed -in \"s/\$USER/anon/g\" output.file" >> $out/bin/simula
      echo "${curl}/bin/curl --data-urlencode errorMsg@output.file https://www.wolframcloud.com/obj/george.w.singer/errorMessage" >> $out/bin/simula
      chmod +x $out/bin/simula

     '';

    devBuildTrue = ''
      cp GetNixGL.sh $out/bin/GetNixGL.sh

      # simula_local
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula_local
      echo "if [ ! -d .import ]; then PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -e; else PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -m; fi" >> $out/bin/simula_local
      chmod +x $out/bin/simula_local

      # simula_gdb
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) gdb -x ./.gdbinit ./submodules/godot/bin/godot.x11.tools.64" >> $out/bin/simula_gdb
      echo "cat gdb.txt" >> $out/bin/simula_gdb
      chmod +x $out/bin/simula_gdb

      # simula_rr_record
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr record ./submodules/godot/bin/godot.x11.tools.64 --args -m" >> $out/bin/simula_rr_record
      chmod +x $out/bin/simula_rr_record

      # simula_rr_replay
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr -M replay \"\$@\"" >> $out/bin/simula_rr_replay
      chmod +x $out/bin/simula_rr_replay

      # simula_apitrace
      echo "rm *.trace" >> $out/bin/simula_apitrace
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) apitrace trace --api gl ./submodules/bin/godot.x11.tools.64" >> $out/bin/simula_apitrace
      echo "apitrace dump *.trace | grep glTex > glTex.trace" >> $out/bin/simula_apitrace
      chmod +x $out/bin/simula_apitrace

      # rootston
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/rootston
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ./submodules/wlroots-dev/build/rootston/rootston \"\$@\"" >> $out/bin/rootston
      chmod +x $out/bin/rootston

      # rootston_rr_record
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr record ./submodules/wlroots-dev/build/rootston/rootston" >> $out/bin/rootston_rr_record
      chmod +x $out/bin/rootston_rr_record

      # rootston_rr_replay
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr -M replay \"\$@\"" >> $out/bin/rootston_rr_replay
      chmod +x $out/bin/rootston_rr_replay
     '';

    devBuildScript = if (devBuild == true) then devBuildTrue else devBuildFalse;

    # Ensure xfce4-terminal has working fonts on every distribution
    xfce4-terminal-wrapped = writeScriptBin "xfce4-terminal" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${xfce.xfce4-terminal}/bin/xfce4-terminal
      '';

    midori-wrapped = writeScriptBin "midori" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${midori}/bin/midori
      '';

    simulaPackages = if devBuild == true then [] else [ godot godot-haskell-plugin ];
    linkGHP = if devBuild == true then "" else ''
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/libgodot-haskell-plugin.so $out/bin/libgodot-haskell-plugin.so";
    '';

    simula = stdenv.mkDerivation {
      name = "Simula";
      src = ./utils;
      buildInputs = [ xpra xrdb wmctrl fontconfig glibc-locales xfce4-terminal-wrapped openxr-loader midori-wrapped ] ++ simulaPackages;
      installPhase = ''
      mkdir -p $out/bin
      ln -s ${xpra}/bin/xpra $out/bin/xpra
      ln -s ${xfce4-terminal-wrapped}/bin/xfce4-terminal $out/bin/xfce4-terminal
      ln -s ${xrdb}/bin/xrdb $out/bin/xrdb
      ln -s ${wmctrl}/bin/wmctrl $out/bin/wmctrl
      ln -s ${ffmpeg-full}/bin/ffplay $out/bin/ffplay
      ln -s ${midori-wrapped}/bin/midori $out/bin/midori
      ln -s ${ulauncher}/bin/ulauncher $out/bin/ulauncher
      ln -s ${xsel}/bin/xsel $out/bin/xsel
      ln -s ${mimic}/bin/mimic $out/bin/mimic
      ln -s ${xclip}/bin/xclip $out/bin/xclip
      ln -s ${rr}/bin/rr $out/bin/rr
      ln -s ${dialog}/bin/dialog $out/bin/dialog

      '' + linkGHP + devBuildScript;
    };

in simula
