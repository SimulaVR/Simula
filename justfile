build:
  nix build && \
  just build-monado && \
  just build-wlroots && \
  just build-godot && \
  just build-godot-openxr && \
  source ./utils/Helpers.sh && patchGodotWlroots && \
  just build-godot-haskell-plugin && \
  just switch-to-local

direnv_allow:
  find . -name '.envrc' -execdir direnv allow \;

direnv_deny:
  find . -name '.envrc' -execdir direnv deny \;

check-commit submodule_name:
  source ./utils/checkCommitConsistency.sh && checkCommitConsistency {{submodule_name}}

check-commits:
  source ./utils/checkCommitConsistency.sh && checkCommitConsistencies

build-wlroots:
  nix develop ./submodules/wlroots# --command bash -c "cd ./submodules/wlroots && just build"

build-godot:
  nix develop ./submodules/godot# --command bash -c "cd ./submodules/godot && just build"

build-godot-watch:
  nix develop ./submodules/godot# --command bash -c "cd ./submodules/godot && just build-watch"

build-godot-openxr:
  nix develop ./submodules/godot-openxr# --command bash -c "cd ./submodules/godot-openxr && just build && just install"

build-godot-haskell-plugin:
  cd ./addons/godot-haskell-plugin && just build && cd -

clean-godot-haskell-plugin:
  cd ./addons/godot-haskell-plugin && just clean && cd -

switch-to-nix:
  cd ./addons/godot-haskell-plugin && just switch-to-nix && cd -

switch-to-local:
  cd ./addons/godot-haskell-plugin && just switch-to-local && cd -

build-godot-haskell-plugin-watch:
  cd ./addons/godot-haskell-plugin && just switch-to-local && just build-watch && cd -

build-monado:
  nix develop ./submodules/monado?submodules=1#default --command bash -c "cd ./submodules/monado && just build"

build-monado-watch:
  nix develop ./submodules/monado?submodules=1#default --command bash -c "cd ./submodules/monado && just build"

run-monado:
  ./result/bin/simula-monado-service --local

run:
  ./result/bin/simula --local
