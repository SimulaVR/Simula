{ pkgs ? import ./pinned-nixpkgs.nix {} }:

let
  nix-cmds = [
    "nix"
    "nix-build"
    "nix-shell"
    "nix-env"
    "nix-store"
    "nix-instantiate"
    "nix-collect-garbage"
    "nix-copy-closure"
    "nix-daemon"
    "nix-hash"
    "nix-prefetch-url"
    "nix-channel"
  ];

  wrap-cmd = cmd: pkgs.writeShellScriptBin cmd ''
    export PATH="${pkgs.nix}/bin:$PATH"
    exec ${pkgs.nix}/bin/${cmd} "$@"
  '';

in pkgs.symlinkJoin {
  name = "nix-forced-version";
  paths = map wrap-cmd nix-cmds;
}