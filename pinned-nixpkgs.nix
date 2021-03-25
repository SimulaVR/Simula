import (builtins.fetchGit {
  url = "https://github.com/KaneTW/nixpkgs.git";
  # rev = "bc7d101e7ec14b97f81cca0e88cc21f951ad56de"; # needed for NUC
  rev = "5df05c902cde398e056eb6271d5fe13e418db4c6";
  ref = "refs/heads/nixpkgs-unstable";
  name = "nixpkgs-unstable-custom-2021-03-09";
})

