import (builtins.fetchGit {
  url = "https://github.com/SimulaVR/nixpkgs.git";
  rev = "6925fce0512127e4a5c1d3a0e79b8e0a5c411472"; # needed for NUC
  # rev = "7110179c41e0a866b05187efcbfdac5035d5fb03";
  ref = "refs/heads/nuc";
  name = "nixpkgs-unstable-custom-nuc";
})

