import (builtins.fetchGit {
  url = "https://github.com/KaneTW/nixpkgs.git";
  # rev = "bc7d101e7ec14b97f81cca0e88cc21f951ad56de"; # needed for NUC
  rev = "7110179c41e0a866b05187efcbfdac5035d5fb03";
  ref = "refs/heads/fpic";
  name = "nixpkgs-unstable-custom-profiling";
})

