{
  callPackage,
  onNixOS,
  devBuild,
  profileBuild ? false,
}:

callPackage ./Simula.nix { inherit onNixOS devBuild profileBuild; }
