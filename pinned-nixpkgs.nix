import (builtins.fetchGit {
  url = "https://github.com/SimulaVR/nixpkgs.git";
  rev = "d3d0c5d418b17f2f9566f319e537354d0adfee47"; # needed for NUC
  # rev = "7110179c41e0a866b05187efcbfdac5035d5fb03";
  ref = "refs/heads/nuc";
  name = "nixpkgs-unstable-custom-nuc";
})

