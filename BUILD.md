## 1 Building Simula
### 1.1 Building with Nix

Building Simula is easiest with `nix`. 
If you don't already have `nix` installed, you can get it from your distro's package manager, or run:

```
curl https://nixos.org/nix/install | sh
```

Then pull in the required submodules:

```
$ git submodule init
$ git submodule update --init --recursive
```

Then initiate the build:

```
$ stack --nix build
$ source ./swrast.sh # only needs to be run once
```

Nix automatically downloads every non-Haskell dependency for this project and places them in `/nix/store` in such a way that they don't conflict with your current distro's libraries. Running `stack` with these flags tells it how to find these libraries. The `swrast.sh` script tells nix how to find your system's OpenGL drivers.

### 1.2 Building without Nix

This is highly unrecommended, but see [here](./BUILD_WITHOUT_NIX.md) for some rough guidelines about what libraries you need to build this project without nix.

## 2 Launching Simula

At present, Simula contains two compositor executables: `base-compositor` and `vive-compositor`. The `base-compositor` (successfully) launches Simula in a normal window for development purposes. You don't need a VR headset to launch it. The `vive-compositor`, however, attempts (and -- at present -- fails) to launch the compositor in an HTC Vive.

### 2.1 Base-Compositor

To launch the `base-compositor`, run

```
$ stack [--nix --no-exec-pure] exec base-compositor # include the flags only if you built w/nix
```

This should launch a white screen compositor. In order to see more, you have to launch a Wayland application, such as

```
$ weston-terminal
```

### 2.2 Vive-Compositor

You will need two terminals to launch this compositor. In the first terminal, you must launch the OSVR server:

```
$ nix-shell ./shell.nix                             # not needed unless you built the project with nix
$ osvr_server ./config/ViveDirectMode.json          # or use ViveExtendedMode.json for extended mode
```

In the second terminal, launch the `vive-compositor`:

```
$ stack [--nix --no-exec-pure] exec vive-compositor # include the flags only if you built w/nix
```

**NOTE:** For the `vive-compositor` only, you will need `nvidia-381.26.13` (or higher) video drivers. Unfortunately, nix cannot provide those, so you will have to get them from your system's package manager.

## 3 Troubleshooting

For assistance, [go here](https://gitter.im/SimulaVR/Simula).
