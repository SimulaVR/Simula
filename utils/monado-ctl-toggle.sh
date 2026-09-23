#!/usr/bin/env bash
set -euo pipefail

# The following is (vibecoded) helper script which wraps `monado-ctl` (which can be built with `just build-monado-ctl`).
# If there is more than one active monado/OpenXR client, then executing this script toggles between them.
# This is useful if you want to e.g. use Simula to develop a fullscreen OpenXR client (while being able to switch to that
# client to see how your changes were reflected). It's likely also useful for running more than one Simula instance
# at once.


simula_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
ctl="$simula_dir/monado-ctl"

# Prefer the control tool built with the running service, as in desperado.sh.
mapfile -t service_pids < <(pgrep -u "$(id -u)" -x monado-service || true)
if (( ${#service_pids[@]} == 1 )); then
    service_exe="$(readlink -f "/proc/${service_pids[0]}/exe" || true)"
    if [[ -z "$service_exe" ]]; then
        IFS= read -r -d '' service_exe < "/proc/${service_pids[0]}/cmdline" || true
    fi
    service_bin="${service_exe%/*}"
    if [[ -x "$service_bin/monado-ctl" ]]; then
        ctl="$service_bin/monado-ctl"
    fi
fi

if [[ ! -x "$ctl" ]]; then
    printf 'Build monado-ctl first: just build-monado-ctl\n' >&2
    exit 1
fi

clients="$("$ctl")"
target="$(awk '
    /^Devices:/ { exit }
    $1 == "id:" {
        id = $2
        active = visible = 0
        overlay = 1
        for (i = 3; i < NF; i++) {
            if ($i == "act:") active = $(i + 1)
            if ($i == "disp:") visible = $(i + 1)
            if ($i == "ovly:") overlay = $(i + 1)
        }
        if (active != 1 || overlay != 0) next

        # monado-ctl puts the complete application name in the final tab field.
        name = $0
        sub(/^.*\t/, "", name)
        sub(/[[:space:]]+$/, "", name)
        if (name == "Simula") {
            simula = id
        } else {
            if (other == "" || id + 0 < other + 0) other = id
            if (visible == 1) other_visible = 1
        }
    }
    END {
        # No switch without both Simula and an alternative active application.
        if (simula != "" && other != "") {
            if (other_visible) print simula
            else print other
        }
    }
' <<< "$clients")"

[[ -n "$target" ]] || exit 0
[[ "$target" =~ ^[0-9]+$ ]] || exit 1
"$ctl" -p "$target"
printf 'Monado primary client switched to %s\n' "$target"
