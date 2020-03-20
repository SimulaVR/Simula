{ stdenv, fetchFromGitHub, haskellPackages, callPackage, buildEnv, xrdb, wmctrl, SDL2, vulkan-loader, lib, driverCheck ? "", xwayland, xkbcomp, ghc, ffmpeg-full, xfce, devBuild }:
let
    godot = callPackage ./submodules/godot/godot.nix { devBuild = devBuild; driverCheck = driverCheck; pkgs = import ./pinned-nixpkgs.nix; };
    godot-api = "${godot}/bin/api.json";
    godot-haskell = haskellPackages.callPackage ./submodules/godot-haskell/default.nix { api-json = godot-api; };
    godot-haskell-plugin = haskellPackages.callPackage ./addons/godot-haskell-plugin/godot-haskell-plugin.nix { devBuild = devBuild; driverCheck = driverCheck; pkgs = import ./pinned-nixpkgs.nix; };

    driverCheckList = lib.splitString " " driverCheck;
    nvidia-version = if ((builtins.head driverCheckList) == "nvidia") then (builtins.elemAt driverCheckList 1) else null;
    nvidia-hash = if ((builtins.head driverCheckList) == "nvidia") then (builtins.elemAt driverCheckList 2) else null;
    nixVulkanNvidia = ((import ./submodules/godot/nixGL.nix) { nvidiaVersion = "${nvidia-version}"; nvidiaHash = "${nvidia-hash}"; pkgs = import ./pinned-nixpkgs.nix; }).nixVulkanNvidia;
    nixGLIntel = ((import ./submodules/godot/nixGL.nix) { pkgs = import ./pinned-nixpkgs.nix; }).nixGLIntel;
    nixGLRes = if ((builtins.head driverCheckList) == "nixos") then " " else (if ((builtins.head driverCheckList) == "nvidia") then " ${nixVulkanNvidia}/bin/nixVulkanNvidia " else " ${nixGLIntel}/bin/nixGLIntel ");

    ghc-version = ghc.version;

    xpra = callPackage ./nix/xpra/default.nix { };

    devBuildFalse = ''

      ln -s ${godot}/bin/godot.x11.opt.debug.64 $out/bin/godot.x11.opt.debug.64
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64
      ln -s ${godot}/bin/godot.x11.opt.64 $out/bin/godot.x11.opt.64
      echo "if [ ! -d .import ]; then LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader}/lib LC_ALL=C.UTF-8 '' + nixGLRes + '' ${godot}/bin/godot.x11.tools.64 --export \"Linux/X11\" ./result/bin/SimulaExport; fi" > $out/bin/simula
      echo "PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader}/lib LC_ALL=C.UTF-8 '' + nixGLRes + '' ${godot}/bin/godot.x11.opt.64 -m" >> $out/bin/simula
      chmod +x $out/bin/simula

     '';

    devBuildTrue = ''
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64

      echo "if [ ! -d .import ]; then PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader}/lib LC_ALL=C.UTF-8 '' + nixGLRes + '' ${godot}/bin/godot.x11.tools.64 -e; else PATH=${xwayland}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader}/lib LC_ALL=C.UTF-8 '' + nixGLRes + '' ${godot}/bin/godot.x11.tools.64 -m; fi" > $out/bin/simula
      chmod +x $out/bin/simula
     '';

    devBuildScript = if (devBuild == true) then devBuildTrue else devBuildFalse;

    # xvfb-run = callPackage ./submodules/godot/xvfb-run.nix { };

    simula = stdenv.mkDerivation {
      name = "Simula";
      src = ./utils;
      buildInputs = [ godot godot-haskell-plugin xpra xrdb wmctrl ];
      installPhase = ''
      mkdir -p $out/bin
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/libgodot-haskell-plugin.so $out/bin/libgodot-haskell-plugin.so
      ln -s ${xfce.xfce4-terminal}/bin/xfce4-terminal $out/bin/xfce4-terminal
      ln -s ${xpra}/bin/xpra $out/bin/xpra
      ln -s ${xrdb}/bin/xrdb $out/bin/xrdb
      ln -s ${wmctrl}/bin/wmctrl $out/bin/wmctrl
      ln -s ${ffmpeg-full}/bin/ffplay $out/bin/ffplay

      '' + devBuildScript;
    };

in simula
