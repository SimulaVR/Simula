build:
  nix build && \
  just build-monado && \
  just build-wlroots && \
  just build-godot && \
  just build-godot-openxr && \
  source ./utils/Helpers.sh && patchGodotWlroots && \
  just build-godot-haskell-plugin && \
  just switch-to-local

build-profiling:
  nix build && \
  just build-monado && \
  just build-wlroots && \
  just build-godot && \
  just build-godot-openxr && \
  source ./utils/Helpers.sh && patchGodotWlroots && \
  just build-godot-haskell-plugin-profiling && \
  just switch-to-local-profiling

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

build-godot-haskell-plugin-profiling:
  cd ./addons/godot-haskell-plugin && just build-profiling && cd -

clean-godot-haskell-plugin:
  cd ./addons/godot-haskell-plugin && just clean && cd -

switch-to-nix:
  cd ./addons/godot-haskell-plugin && just switch-to-nix && cd -

switch-to-local:
  cd ./addons/godot-haskell-plugin && just switch-to-local && cd -

switch-to-local-profiling:
  cd ./addons/godot-haskell-plugin && just switch-to-local-profiling && cd -

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

run-profiling:
  ts="$(date -u +%Y%m%dT%H%M%SZ)" && \
  out_dir="./profiling/godot-haskell-plugin/${ts}" && \
  mkdir -p "$out_dir" && \
  echo "Profiling output directory: $out_dir" && \
  SIMULA_HS_PROFILE_PREFIX="$out_dir/godot-haskell-plugin" \
  ./result/bin/simula --local 2>&1 | tee "$out_dir/simula.log"
