## 1 Building
### 1.1 Building with Nix

The easiest way to build Simula is to install `nix` and run

```
$ stack --nix build
$ source ./swrast.sh # only needs to be run once
```

You can then launch SimulaHS via

```
$ stack --nix --no-nix-pure exec simple-compositor
```

If you don't have `nix` installed, you can get it from your distro's package manager, or run

```
curl https://nixos.org/nix/install | sh
```

### 1.2 Building without Nix

See [here](./BUILD_WITHOUT_NIX.md).

## 2 Launching

The project contains 2 compositors:

1. **base-compositor (runs):** Launches the compositor in a desktop window for development purpooses.

```
$ stack [--nix --no-exec-pure] exec base-compositor # include the flags if you built w/nix
```

2. **vive-compositor (doesn't currently run):** Attempts (and fails) to the compositor in an HTC Vive headset. You will need two terminals to launch this compositor: one to launch the OSVR server and the other to launch the actual vive-compositor.

```
$ nix-shell shell.nix              # not needed unless you built the project with nix
$ osvr_server ./config_direct.json # or use config_extended.json for extended mode
$ stack exec vive-compositor       # launch this in a second terminal
```
