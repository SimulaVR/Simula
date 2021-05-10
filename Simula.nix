{ stdenv, fetchFromGitHub, haskellPackages, callPackage, buildEnv, xrdb, wmctrl, SDL2, lib, onNixOS ? false, xwayland, xkbcomp, ghc, ffmpeg-full, midori, xfce, devBuild, fontconfig, glibcLocales, dejavu_fonts, writeScriptBin, coreutils, curl, vulkan-loader, mimic, xsel, xclip, dialog, synapse, openxr-loader, xpra, valgrind, xorg, writeShellScriptBin, python3, awscli, wayland, wayland-protocols }:
let

    /* Modify a stdenv so that it produces debug builds; that is,
      binaries have debug info, and compiler optimisations are
      disabled. */
    keepDebugInfo = stdenv: stdenv //
      { mkDerivation = args: stdenv.mkDerivation (args // {
          dontStrip = true;
          NIX_CFLAGS_COMPILE = toString (args.NIX_CFLAGS_COMPILE or "") + " -g -ggdb -Og";
        });
      };
    stdenvRes = if devBuild then (keepDebugInfo stdenv) else stdenv;

    xwayland-dev = callPackage ./nix/xwayland/xwayland.nix { stdenv = stdenvRes; };
    libxcb-dev = xorg.libxcb.override { stdenv = stdenvRes; };
	  wayland-dev = wayland.override { stdenv = stdenvRes; };
    wayland-protocols-dev = wayland-protocols.override { stdenv = stdenvRes; };
    wlroots-dev = callPackage ./submodules/wlroots/wlroots.nix { stdenv = stdenvRes; };

    vulkan-loader-custom = if onNixOS then vulkan-loader else (callPackage ./nix/vulkan-loader.nix { });
    glibc-locales = glibcLocales;
    godot = callPackage ./submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; };
    godot-api = "${godot}/bin/api.json";
    godot-haskell = haskellPackages.callPackage ./submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };
    godot-haskell-plugin = haskellPackages.callPackage ./addons/godot-haskell-plugin/godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ./pinned-nixpkgs.nix; godot = godot; godot-haskell = godot-haskell; };

    ghc-version = ghc.version;

    rr = callPackage ./nix/rr/unstable.nix {};

    devBuildFalse = ''
      cp ./utils/GetNixGL.sh $out/bin/GetNixGL.sh
      ln -s ${godot}/bin/godot.x11.opt.debug.64 $out/bin/godot.x11.opt.debug.64
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64
      ln -s ${godot}/bin/godot.x11.opt.64 $out/bin/godot.x11.opt.64
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula
      echo "source ./utils/Helpers.sh && updateEmail" >> $out/bin/simula
      echo "mkdir -p log" >> $out/bin/simula
      echo "mkdir -p config" >> $out/bin/simula
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib \$(./utils/GetNixGL.sh) ${godot}/bin/godot.x11.opt.debug.64 -m 2>&1 | ${coreutils}/bin/tee ./log/output.file" >> $out/bin/simula
      echo "sed -in \"s/\$USER/anon/g\" ./log/output.file" >> $out/bin/simula
      echo "touch ./config/email && cat ./config/email >> ./log/output.file" >> $out/bin/simula
      echo "${curl}/bin/curl --data-urlencode errorMsg@./log/output.file https://www.wolframcloud.com/obj/george.w.singer/errorMessage" >> $out/bin/simula
      chmod +x $out/bin/simula

     '';

    devBuildTrue = ''
      cp ./utils/GetNixGL.sh $out/bin/GetNixGL.sh

      # simula_local
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/simula_local
      echo "mkdir -p log" >> $out/bin/simula
      echo "mkdir -p config" >> $out/bin/simula
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib LD_PRELOAD=./submodules/wlroots/build/libwlroots.so.0 \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -m --print-fps" >> $out/bin/simula_local
      chmod +x $out/bin/simula_local

      # simula_gdb
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib LD_PRELOAD=./submodules/wlroots/build/libwlroots.so.0 \$(./utils/GetNixGL.sh) gdb -x ./.gdbinit ./submodules/godot/bin/godot.x11.tools.64" >> $out/bin/simula_gdb
      echo "cat gdb.txt" >> $out/bin/simula_gdb
      chmod +x $out/bin/simula_gdb

      # simula_rr_record
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib LD_PRELOAD=./submodules/wlroots/build/libwlroots.so.0 \$(./utils/GetNixGL.sh) ${rr}/bin/rr record -i SIGUSR1 ./submodules/godot/bin/godot.x11.tools.64 --args -m" >> $out/bin/simula_rr_record
      chmod +x $out/bin/simula_rr_record

      # simula_rr_replay
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib LD_PRELOAD=./submodules/wlroots/build/libwlroots.so.0 \$(./utils/GetNixGL.sh) ${rr}/bin/rr -M replay \"\$@\"" >> $out/bin/simula_rr_replay
      chmod +x $out/bin/simula_rr_replay

      # simula_apitrace
      echo "rm *.trace" >> $out/bin/simula_apitrace
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib LD_PRELOAD=./submodules/wlroots/build/libwlroots.so.0 \$(./utils/GetNixGL.sh) apitrace trace --api gl ./submodules/bin/godot.x11.tools.64" >> $out/bin/simula_apitrace
      echo "apitrace dump *.trace | grep glTex > glTex.trace" >> $out/bin/simula_apitrace
      chmod +x $out/bin/simula_apitrace

      # rootston
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/rootston
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ./submodules/wlroots/build/rootston/rootston \"\$@\"" >> $out/bin/rootston
      chmod +x $out/bin/rootston

      # rootston_rr_record
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr record ./submodules/wlroots/build/rootston/rootston" >> $out/bin/rootston_rr_record
      chmod +x $out/bin/rootston_rr_record

      # rootston_rr_replay
      echo "_RR_TRACE_DIR=./rr PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ${rr}/bin/rr -M replay \"\$@\"" >> $out/bin/rootston_rr_replay
      chmod +x $out/bin/rootston_rr_replay
     '';

    devBuildScript = if (devBuild == true) then devBuildTrue else devBuildFalse;

    # Ensure xfce4-terminal has working fonts on every distribution
    xfce4-terminal-wrapped = writeScriptBin "xfce4-terminal" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${xfce.xfce4-terminal}/bin/xfce4-terminal "$@"
      '';

    midori-wrapped = writeScriptBin "midori" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${midori}/bin/midori
      '';

    simulaPackages = if devBuild == true then [] else [ godot godot-haskell-plugin ];
    linkGHP = if devBuild == true then "" else ''
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/libgodot-haskell-plugin.so $out/bin/libgodot-haskell-plugin.so;
    '';

    rrSources = writeShellScriptBin "rr_sources" ''
      RR_LOG=all:debug ./result/bin/rr sources \
      --substitute=libwlroots.so.0.0.0=$PWD/submodules/wlroots/backend \
      --substitute=libwayland-client.so.0.3.0=$PWD/result/srcs/wayland/src \
      --substitute=libwayland-egl.so.1.0.0=$PWD/result/srcs/wayland/egl \
      --substitute=libwayland-server.so.0.1.0=$PWD/result/srcs/wayland/src \
      --substitute=libxcb.so.1.1.0=$PWD/result/srcs/libxcb/src \
      --substitute=Xwayland=$PWD/result/srcs/xwayland/doc \
      ./rr/latest-trace \
      > sources.txt 2>&1
    '';

    pernoscoSubmit = writeShellScriptBin "pernosco_submit" ''
      PATH=${awscli}/bin:./result/bin:$PATH ${python3}/bin/python3 ./submodules/pernosco-submit/pernosco-submit \
        -x \
      upload \
      --title $2 \
      --substitute=libwlroots.so.0.0.0=$PWD/submodules/wlroots/backend \
      --substitute=libwayland-client.so.0.3.0=$PWD/result/srcs/wayland/src \
      --substitute=libwayland-egl.so.1.0.0=$PWD/result/srcs/wayland/egl \
      --substitute=libwayland-server.so.0.1.0=$PWD/result/srcs/wayland/src \
      --substitute=libxcb.so.1.1.0=$PWD/result/srcs/libxcb/src \
      --substitute=Xwayland=$PWD/result/srcs/xwayland/doc \
      $1 ./. \
      $PWD/submodules/wlroots \
      $PWD/result/srcs \
      $PWD/result/srcs/wayland/src \
      $PWD/result/srcs/wayland/egl \
      $PWD/result/srcs/libxcb/src \
      $PWD/result/srcs/xwayland/doc \
      ${wayland-dev} \
      ${wlroots-dev} \
      > pernosco.txt 2>&1
    '';

    simula = stdenv.mkDerivation {
      name = "Simula";
      src = builtins.filterSource (path: type:
           stdenv.lib.cleanSourceFilter path type                 # Necessary to avoid nix "out of memory" errors
        && (! (stdenv.lib.hasSuffix ".import" (baseNameOf path))) # Nix shouldn't compare about *.imports and their assets
        && (! (stdenv.lib.hasSuffix ".md5" (baseNameOf path)))    # "
        && (! (stdenv.lib.hasSuffix ".stex" (baseNameOf path)))   # "
        && (baseNameOf (builtins.dirOf path) != ".import")        # "
        && (baseNameOf (builtins.dirOf path) != "log")            # Don't let log/* files confuse cachix
        && (baseNameOf (builtins.dirOf path) != "config")         # Don't let user config file alterations confuse cachix
        && (baseNameOf (builtins.dirOf path) != "png")            # Don't let user pictures confuse cachix
        && (baseNameOf (builtins.dirOf path) != "rr")             # Don't let rr traces confuse cachix
        # && (baseNameOf path != ".git")                          # Nix/cachix already isn't confused by this
        # && (baseNameOf path != "result")                        # "
      ) ./.;

      buildInputs = [ xpra xrdb wmctrl fontconfig glibc-locales xfce4-terminal-wrapped openxr-loader midori-wrapped valgrind pernoscoSubmit ] ++ simulaPackages;
      installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/srcs
      ln -s ${xpra}/bin/xpra $out/bin/xpra
      ln -s ${xfce4-terminal-wrapped}/bin/xfce4-terminal $out/bin/xfce4-terminal
      ln -s ${xrdb}/bin/xrdb $out/bin/xrdb
      ln -s ${wmctrl}/bin/wmctrl $out/bin/wmctrl
      ln -s ${ffmpeg-full}/bin/ffplay $out/bin/ffplay
      ln -s ${midori-wrapped}/bin/midori $out/bin/midori
      ln -s ${synapse}/bin/synapse $out/bin/synapse
      ln -s ${xsel}/bin/xsel $out/bin/xsel
      ln -s ${mimic}/bin/mimic $out/bin/mimic
      ln -s ${xclip}/bin/xclip $out/bin/xclip
      ln -s ${rr}/bin/rr $out/bin/rr
      ln -s ${dialog}/bin/dialog $out/bin/dialog
      ln -s ${curl}/bin/curl $out/bin/curl
      ln -s ${valgrind}/bin/valgrind $out/bin/valgrind


      mkdir -p $out/srcs/xwayland
      tar -xvf ${xwayland-dev.src} --directory $out/srcs/xwayland --strip-components=1
      mkdir -p $out/srcs/libxcb
      tar -xvf ${libxcb-dev.src} --directory $out/srcs/libxcb --strip-components=1
      mkdir -p $out/srcs/wayland
      tar -xvf ${wayland-dev.src} --directory $out/srcs/wayland --strip-components=1
      ln -s ${pernoscoSubmit}/bin/pernosco_submit $out/bin/pernosco_submit
      ln -s ${rrSources}/bin/rr_sources $out/bin/rr_sources

      '' + linkGHP + devBuildScript;
    };

in simula
