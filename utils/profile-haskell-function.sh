#!/usr/bin/env bash

# Helper script to run a SIMULA_DEBUG_PROFILE_HUD capture and append selected live-HUD fields to a CSV.
#
# Example:
#   ./utils/profile-haskell-function.sh \
#     --test-time 10s \
#     --function "SimulaViewSprite._process" \
#     --test-setup "Launch firefox running YouTube video" \
#     --parse avg_ms,max_ms \
#     ./result/bin/simula --local

test_time="10s"
profile_function=""
test_setup=""
output=""
parse_fields="avg_ms,max_ms"
hud_live_path="./HUD_profile_live.txt"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --test-time)
            test_time="${2:?profile-haskell-function.sh: --test-time requires a value}"
            shift 2
            ;;
        --function)
            profile_function="${2:?profile-haskell-function.sh: --function requires a value}"
            shift 2
            ;;
        --test-setup)
            test_setup="${2:?profile-haskell-function.sh: --test-setup requires a value}"
            shift 2
            ;;
        --output)
            output="${2:?profile-haskell-function.sh: --output requires a value}"
            shift 2
            ;;
        --parse)
            parse_fields="${2:?profile-haskell-function.sh: --parse requires a value}"
            shift 2
            ;;
        --hud-live-path)
            hud_live_path="${2:?profile-haskell-function.sh: --hud-live-path requires a value}"
            shift 2
            ;;
        --help|-h)
            cat <<'EOF'
profile-haskell-function.sh [FLAGS] [COMMAND...]

Flags:
  --test-time TIME       How long to let Simula run before timeout. Default: 10s
  --function NAME        Value passed to SIMULA_DEBUG_PROFILE_ROOT. Required.
  --test-setup TEXT      Free-form note describing the test setup.
  --output FILE          CSV file to append. Default: <function>-profile.csv
  --parse FIELDS         Comma/space-separated HUD_profile_live.txt fields.
                         Default: avg_ms,max_ms
  --hud-live-path FILE   Live HUD file to parse. Default: ./HUD_profile_live.txt

If COMMAND is omitted, defaults to: ./result/bin/simula --local
EOF
            exit 0
            ;;
        --)
            shift
            break
            ;;
        --*)
            echo "profile-haskell-function.sh: unknown flag: $1" >&2
            exit 2
            ;;
        *)
            break
            ;;
    esac
done

if [[ -z "$profile_function" ]]; then
    echo "profile-haskell-function.sh: --function is required" >&2
    exit 2
fi

if [[ -z "$output" ]]; then
    safe_function="$(printf '%s' "$profile_function" | tr -c '[:alnum:]_.-' '_')"
    output="${safe_function}-profile.csv"
fi

command=("$@")
if [[ ${#command[@]} -eq 0 ]]; then
    command=(./result/bin/simula --local)
fi

rm -f "$hud_live_path"

started_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

tty_state=""
if [[ -t 0 ]]; then
    tty_state="$(stty -g 2>/dev/null)"
fi

restore_tty() {
    if [[ -n "$tty_state" ]]; then
        stty "$tty_state" 2>/dev/null || stty sane 2>/dev/null
    fi
}
trap restore_tty EXIT

matching_local_simula_pids() {
    local repo_cwd pid cwd
    repo_cwd="$(pwd -P)"
    pgrep -f 'simula-unwrapped --local|script -qfc .*godot\.x11\.tools\.64|godot\.x11\.tools\.64 -m "\./\."|godot\.x11\.tools\.64 -m \./\.' |
        while read -r pid; do
            [[ "$pid" == "$$" ]] && continue
            cwd="$(readlink -f "/proc/$pid/cwd" 2>/dev/null)" || continue
            [[ "$cwd" == "$repo_cwd" ]] && printf '%s\n' "$pid"
        done
}

pid_in_list() {
    local needle="$1"
    local pid
    shift
    for pid in "$@"; do
        [[ "$pid" == "$needle" ]] && return 0
    done
    return 1
}

cleanup_timed_out_simula_processes() {
    local pid
    local -a pids_to_kill=()
    mapfile -t current_matching_pids < <(matching_local_simula_pids)

    for pid in "${current_matching_pids[@]}"; do
        if ! pid_in_list "$pid" "${preexisting_matching_pids[@]}"; then
            pids_to_kill+=("$pid")
        fi
    done

    [[ "${#pids_to_kill[@]}" -eq 0 ]] && return 0

    echo "profile-haskell-function.sh: timeout reached; terminating local Simula/Godot pids: ${pids_to_kill[*]}" >&2
    kill -TERM "${pids_to_kill[@]}" 2>/dev/null || true
    sleep 1

    for pid in "${pids_to_kill[@]}"; do
        if kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
        fi
    done
}

set +e
mapfile -t preexisting_matching_pids < <(matching_local_simula_pids)
SIMULA_DEBUG_PROFILE_HUD=1 \
SIMULA_DEBUG_PROFILE_ROOT="$profile_function" \
    timeout --foreground "$test_time" "${command[@]}"
exit_code=$?
if [[ "$exit_code" -eq 124 ]]; then
    cleanup_timed_out_simula_processes
fi
restore_tty

ended_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

if [[ ! -s "$hud_live_path" ]]; then
    echo "profile-haskell-function.sh: $hud_live_path was not written or is empty" >&2
    echo "profile-haskell-function.sh: simula exit code was $exit_code" >&2
    exit 1
fi

normalized_fields="$(printf '%s' "$parse_fields" | tr ',;' '  ')"

parsed_values=()
mapfile -t parsed_values < <(
    awk -v fields="$normalized_fields" '
        function trim(s) {
            sub(/^[[:space:]]+/, "", s)
            sub(/[[:space:]]+$/, "", s)
            return s
        }
        BEGIN {
            n = split(fields, requested, /[[:space:]]+/)
        }
        /^updated: / {
            values["hud_updated"] = trim(substr($0, index($0, ":") + 1))
        }
        /^mode: / {
            values["mode"] = trim(substr($0, index($0, ":") + 1))
        }
        /^root: / {
            values["root"] = trim(substr($0, index($0, ":") + 1))
        }
        /^budget_ms: / {
            values["budget_ms"] = trim(substr($0, index($0, ":") + 1))
        }
        /^frame_retention_min_ms: / {
            values["frame_retention_min_ms"] = trim(substr($0, index($0, ":") + 1))
        }
        /^window_s: / {
            values["window_s"] = trim(substr($0, index($0, ":") + 1))
        }
        /^last_frame_ms: / {
            values["last_frame_ms"] = trim(substr($0, index($0, ":") + 1))
        }
        /^worst_recent_frame_ms: / {
            values["worst_recent_frame_ms"] = trim(substr($0, index($0, ":") + 1))
        }
        /^retained_/ {
            key = $1
            sub(/:$/, "", key)
            values[key] = trim(substr($0, index($0, ":") + 1))
        }
        /^frame_id=/ {
            for (i = 1; i <= NF; i++) {
                split($i, kv, "=")
                values["worst_" kv[1]] = kv[2]
            }
        }
        /^top_path=/ {
            values["worst_top_path"] = trim(substr($0, length("top_path=") + 1))
        }
        /^[^-[:space:]]/ && $0 !~ /:/ && $0 !~ /^SIMULA_DEBUG_PROFILE_HUD/ && $0 !~ /^function\/path/ && $0 !~ /^Worst frame/ && first_table_row == "" {
            first_table_row = $0
            metric_tail = trim(substr($0, 73))
            metric_count = split(metric_tail, metric, /[[:space:]]+/)
            if (metric_count >= 6) {
                values["function_path"] = trim(substr($0, 1, 72))
                values["sum_ms"] = metric[1]
                values["avg_ms"] = metric[2]
                values["max_ms"] = metric[3]
                values["calls"] = metric[4]
                values["frames"] = metric[5]
                values["tag"] = metric[6]
            }
        }
        END {
            for (i = 1; i <= n; i++) {
                key = requested[i]
                if (key == "") {
                    continue
                }
                print values[key]
            }
        }
    ' "$hud_live_path"
)

csv_quote() {
    value="$1"
    value="${value//\"/\"\"}"
    printf '"%s"' "$value"
}

header_fields="timestamp_utc,ended_at_utc,test_time,function,test_setup,command,exit_code"
for field in $normalized_fields; do
    [[ -n "$field" ]] && header_fields="${header_fields},${field}"
done

if [[ ! -s "$output" ]]; then
    printf '%s\n' "$header_fields" >> "$output"
fi

{
    csv_quote "$started_at"
    printf ','
    csv_quote "$ended_at"
    printf ','
    csv_quote "$test_time"
    printf ','
    csv_quote "$profile_function"
    printf ','
    csv_quote "$test_setup"
    printf ','
    csv_quote "${command[*]}"
    printf ','
    csv_quote "$exit_code"

    for value in "${parsed_values[@]}"; do
        printf ','
        csv_quote "$value"
    done
    printf '\n'
} >> "$output"

echo "profile-haskell-function.sh: appended $(basename "$output")"
echo "profile-haskell-function.sh: simula exit code was $exit_code"
