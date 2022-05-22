import (builtins.fetchGit {
  url = "https://github.com/SimulaVR/nixpkgs.git";
  rev = "acffaa439d445bf55faa1e2b5de3cadbdc9fbda7"; # needed for NUC
  # rev = "7110179c41e0a866b05187efcbfdac5035d5fb03";
  ref = "refs/heads/nuc";
  name = "nixpkgs-unstable-custom-nuc";
})

