# Building with Nix

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

# Building without Nix

See [here](./BUILD_WITHOUT_NIX.md).
