## 1 Building
### 1.1 Building with Nix

The easiest way to build Simula is to install `nix` and run

```
$ stack --nix build
$ source ./swrast.sh # only needs to be run once
```

If you don't have `nix` installed, you can get it from your distro's package manager, or run

```
curl https://nixos.org/nix/install | sh
```

### 1.2 Building without Nix

Not recommended, but see [here](./BUILD_WITHOUT_NIX.md).

## 2 Launching

The project contains two compositors: base-compositor and vive-compositor.

1. **base-compositor (runs):** Launches the compositor in a desktop window for development purpooses.

```
$ stack [--nix --no-exec-pure] exec base-compositor # include the flags only if you built w/nix
```

2. **vive-compositor (doesn't currently run):** Attempts (and fails) to launch the compositor in an HTC Vive headset. You will need two terminals to launch this compositor. In the first terminal, you must launch the OSVR server:

```
$ nix-shell ./shell.nix                             # not needed unless you built the project with nix
$ osvr_server ./config/ViveDirectMode.json          # or use ViveExtendedMode.json for extended mode
```

In the second terminal, launch the vive-compositor:

```
$ stack [--nix --no-exec-pure] exec vive-compositor # include the flags only if you built w/nix
```
