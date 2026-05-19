#!/usr/bin/env bash

# Run the Haskell Profile HUD helper under perf stat and save a coarse run
# artifact directory. This intentionally avoids sudo so Simula keeps the user's
# normal config/session environment.

set -euo pipefail

test_time="60s"
profile_function=""
test_setup=""
parse_fields="avg_ms,max_ms"
run_dir=""
run_sched_smoke=1

usage() {
    cat <<'EOF'
profile-haskell-function-perf-coarse.sh [FLAGS] [COMMAND...]

Flags:
  --test-time TIME       How long to let Simula run before timeout. Default: 60s
  --function NAME        Value passed to SIMULA_DEBUG_PROFILE_ROOT. Required.
  --test-setup TEXT      Free-form note describing the test setup.
  --parse FIELDS         Comma/space-separated HUD_profile_live.txt fields.
                         Default: avg_ms,max_ms
  --run-dir DIR          Directory for artifacts. Default:
                         ./perf-runs/<utc timestamp>-<function>
  --skip-sched-smoke     Do not run the minimal perf sched decode check.

If COMMAND is omitted, defaults to: ./result/bin/simula --local
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --test-time)
            test_time="${2:?profile-haskell-function-perf-coarse.sh: --test-time requires a value}"
            shift 2
            ;;
        --function)
            profile_function="${2:?profile-haskell-function-perf-coarse.sh: --function requires a value}"
            shift 2
            ;;
        --test-setup)
            test_setup="${2:?profile-haskell-function-perf-coarse.sh: --test-setup requires a value}"
            shift 2
            ;;
        --parse)
            parse_fields="${2:?profile-haskell-function-perf-coarse.sh: --parse requires a value}"
            shift 2
            ;;
        --run-dir)
            run_dir="${2:?profile-haskell-function-perf-coarse.sh: --run-dir requires a value}"
            shift 2
            ;;
        --skip-sched-smoke)
            run_sched_smoke=0
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        --)
            shift
            break
            ;;
        --*)
            echo "profile-haskell-function-perf-coarse.sh: unknown flag: $1" >&2
            exit 2
            ;;
        *)
            break
            ;;
    esac
done

if [[ -z "$profile_function" ]]; then
    echo "profile-haskell-function-perf-coarse.sh: --function is required" >&2
    exit 2
fi

if ! command -v perf >/dev/null 2>&1; then
    echo "profile-haskell-function-perf-coarse.sh: perf is not on PATH" >&2
    exit 127
fi

command_to_run=("$@")
if [[ ${#command_to_run[@]} -eq 0 ]]; then
    command_to_run=(./result/bin/simula --local)
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
repo_dir="$(cd "$script_dir/.." && pwd -P)"
cd "$repo_dir"

safe_function="$(printf '%s' "$profile_function" | tr -c '[:alnum:]_.-' '_')"
timestamp_slug="$(date -u +"%Y%m%dT%H%M%SZ")"
timestamp_iso="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

if [[ -z "$run_dir" ]]; then
    run_dir="./perf-runs/${timestamp_slug}-${safe_function}"
fi

mkdir -p "$run_dir"

profile_csv="$run_dir/profile.csv"
perf_stat_txt="$run_dir/perf-stat.txt"
profile_log="$run_dir/profile-command.log"
summary_md="$run_dir/summary.md"
environment_txt="$run_dir/environment.txt"
command_txt="$run_dir/command.txt"
sched_smoke_txt="$run_dir/perf-sched-smoke.txt"
sched_smoke_data="$run_dir/perf-sched-smoke.data"

events="task-clock,context-switches,cpu-migrations,page-faults,major-faults,cycles,instructions,sched:sched_switch"

quote_words() {
    printf '%q ' "$@"
}

{
    printf 'timestamp_utc=%s\n' "$timestamp_iso"
    printf 'repo_dir=%s\n' "$repo_dir"
    printf 'perf_path=%s\n' "$(command -v perf)"
    printf 'perf_version=%s\n' "$(perf --version)"
    printf 'uname=%s\n' "$(uname -a)"
    if [[ -r /proc/sys/kernel/perf_event_paranoid ]]; then
        printf 'perf_event_paranoid=%s\n' "$(cat /proc/sys/kernel/perf_event_paranoid)"
    fi
    if [[ -r /proc/sys/kernel/kptr_restrict ]]; then
        printf 'kptr_restrict=%s\n' "$(cat /proc/sys/kernel/kptr_restrict)"
    fi
} > "$environment_txt"

{
    quote_words perf stat -o "$perf_stat_txt" -e "$events" --
    quote_words ./utils/profile-haskell-function.sh \
        --test-time "$test_time" \
        --function "$profile_function" \
        --test-setup "$test_setup" \
        --parse "$parse_fields" \
        --output "$profile_csv" \
        "${command_to_run[@]}"
    printf '\n'
} > "$command_txt"

set +e
perf stat -o "$perf_stat_txt" -e "$events" -- \
    ./utils/profile-haskell-function.sh \
        --test-time "$test_time" \
        --function "$profile_function" \
        --test-setup "$test_setup" \
        --parse "$parse_fields" \
        --output "$profile_csv" \
        "${command_to_run[@]}" \
    > "$profile_log" 2>&1
perf_exit_code=$?
set -e

if [[ ! -e "$perf_stat_txt" ]]; then
    printf 'perf stat did not write %s\n' "$perf_stat_txt" > "$perf_stat_txt"
fi

for hud_path in \
    HUD_profile_live.txt \
    HUD_profile_recent_missed_frames.txt \
    HUD_profile_slow_frames.txt \
    HUD_profile_slow_frames.csv \
    HUD_profile_all_frames.csv \
    HUD_profile_run.log
do
    if [[ -e "$hud_path" ]]; then
        cp "$hud_path" "$run_dir/"
    fi
done

sched_record_exit="skipped"
sched_script_exit="skipped"
sched_latency_exit="skipped"
if [[ "$run_sched_smoke" -eq 1 ]]; then
    {
        printf 'Minimal scheduler trace decode smoke test.\n'
        printf 'Command: '
        quote_words perf sched record -o "$sched_smoke_data" -- sleep 0.2
        printf '\n\n'
    } > "$sched_smoke_txt"

    set +e
    perf sched record -o "$sched_smoke_data" -- sleep 0.2 >> "$sched_smoke_txt" 2>&1
    sched_record_exit=$?
    {
        printf '\nperf sched record exit_code=%s\n\n' "$sched_record_exit"
        printf 'Command: '
        quote_words perf script -i "$sched_smoke_data"
        printf '\n\n'
    } >> "$sched_smoke_txt"
    perf script -i "$sched_smoke_data" >> "$sched_smoke_txt" 2>&1
    sched_script_exit=$?
    {
        printf '\nperf script exit_code=%s\n\n' "$sched_script_exit"
        printf 'Command: '
        quote_words perf sched latency -i "$sched_smoke_data"
        printf '\n\n'
    } >> "$sched_smoke_txt"
    perf sched latency -i "$sched_smoke_data" >> "$sched_smoke_txt" 2>&1
    sched_latency_exit=$?
    printf '\nperf sched latency exit_code=%s\n' "$sched_latency_exit" >> "$sched_smoke_txt"
    set -e
fi

extract_perf_value() {
    local metric="$1"
    awk -v metric="$metric" '
        $2 == metric {
            gsub(/,/, "", $1)
            print $1
            exit
        }
        $3 == metric {
            gsub(/,/, "", $1)
            print $1
            exit
        }
    ' "$perf_stat_txt"
}

task_clock_ms="$(extract_perf_value "task-clock")"
context_switches="$(extract_perf_value "context-switches")"
cpu_migrations="$(extract_perf_value "cpu-migrations")"
page_faults="$(extract_perf_value "page-faults")"
major_faults="$(extract_perf_value "major-faults")"
cycles="$(extract_perf_value "cycles")"
instructions="$(extract_perf_value "instructions")"
sched_switches="$(extract_perf_value "sched:sched_switch")"
elapsed_s="$(
    awk '
        $2 == "seconds" && $3 == "time" && $4 == "elapsed" {
            gsub(/,/, "", $1)
            print $1
            exit
        }
    ' "$perf_stat_txt"
)"
cpus_utilized="$(
    awk '
        $3 == "task-clock" {
            for (i = 1; i <= NF; i++) {
                if ($i == "#") {
                    print $(i + 1)
                    exit
                }
            }
        }
    ' "$perf_stat_txt"
)"

derived_rows="$(
    awk \
        -v elapsed_s="$elapsed_s" \
        -v task_clock_ms="$task_clock_ms" \
        -v context_switches="$context_switches" \
        -v cpu_migrations="$cpu_migrations" \
        -v page_faults="$page_faults" \
        -v major_faults="$major_faults" \
        -v cycles="$cycles" \
        -v instructions="$instructions" \
        -v sched_switches="$sched_switches" \
        -v cpus_utilized="$cpus_utilized" \
        -v perf_exit="$perf_exit_code" '
        function row(name, value) {
            if (value == "") {
                value = "n/a"
            }
            printf "| %s | %s |\n", name, value
        }
        BEGIN {
            row("perf exit code", perf_exit)
            row("elapsed wall time", elapsed_s == "" ? "" : sprintf("%.3f s", elapsed_s + 0))
            row("task-clock", task_clock_ms == "" ? "" : sprintf("%.3f ms", task_clock_ms + 0))
            row("average CPUs utilized", cpus_utilized)
            row("context switches", context_switches)
            if (elapsed_s + 0 > 0 && context_switches != "") {
                row("context switches per elapsed second", sprintf("%.2f", (context_switches + 0) / (elapsed_s + 0)))
            }
            row("sched:sched_switch events", sched_switches)
            row("CPU migrations", cpu_migrations)
            row("page faults", page_faults)
            row("major faults", major_faults)
            row("cycles", cycles)
            row("instructions", instructions)
            if (cycles + 0 > 0 && instructions != "") {
                row("instructions per cycle", sprintf("%.3f", (instructions + 0) / (cycles + 0)))
            }
        }
    '
)"

profile_row=""
if [[ -s "$profile_csv" ]]; then
    profile_row="$(tail -n 1 "$profile_csv")"
fi

{
    printf '# Coarse perf profile\n\n'
    printf 'Run directory: `%s`\n\n' "$run_dir"
    printf 'Timestamp UTC: `%s`\n\n' "$timestamp_iso"
    printf 'Function: `%s`\n\n' "$profile_function"
    printf 'Test setup: `%s`\n\n' "$test_setup"
    printf 'Command:\n\n'
    printf '```sh\n'
    cat "$command_txt"
    printf '```\n\n'

    printf '## Coarse counters\n\n'
    printf '| Metric | Value |\n'
    printf '| --- | ---: |\n'
    printf '%s\n\n' "$derived_rows"

    printf 'These are run-level counters. They can show scheduler churn, migrations, faulting, and total on-CPU task-clock, but they do not identify the exact off-CPU interval for one HUD scope without a readable scheduler trace.\n\n'

    printf '## HUD profile row\n\n'
    if [[ -n "$profile_row" ]]; then
        printf '```csv\n'
        head -n 1 "$profile_csv"
        printf '%s\n' "$profile_row"
        printf '```\n\n'
    else
        printf 'No HUD profile CSV row was written. See `%s`.\n\n' "$(basename "$profile_log")"
    fi

    printf '## perf stat raw output\n\n'
    printf '```text\n'
    cat "$perf_stat_txt"
    printf '```\n\n'

    printf '## Scheduler trace smoke test\n\n'
    printf '| Check | Exit code |\n'
    printf '| --- | ---: |\n'
    printf '| perf sched record | %s |\n' "$sched_record_exit"
    printf '| perf script decode | %s |\n' "$sched_script_exit"
    printf '| perf sched latency decode | %s |\n\n' "$sched_latency_exit"
    printf 'If decode exits non-zero with `incompatible file format` or `broken or missing trace data`, the local `perf sched` trace path is not usable yet. Use `perf-stat.txt` for coarse evidence and fix the sched trace path before relying on `perf sched timehist`.\n\n'

    printf '## Files\n\n'
    printf '%s\n' "- \`$(basename "$perf_stat_txt")\`"
    printf '%s\n' "- \`$(basename "$profile_log")\`"
    printf '%s\n' "- \`$(basename "$profile_csv")\`"
    printf '%s\n' "- \`$(basename "$environment_txt")\`"
    if [[ "$run_sched_smoke" -eq 1 ]]; then
        printf '%s\n' "- \`$(basename "$sched_smoke_txt")\`"
        printf '%s\n' "- \`$(basename "$sched_smoke_data")\`"
    fi
} > "$summary_md"

echo "profile-haskell-function-perf-coarse.sh: wrote $run_dir"
echo "profile-haskell-function-perf-coarse.sh: summary $summary_md"
echo "profile-haskell-function-perf-coarse.sh: perf/profile exit code was $perf_exit_code"

exit "$perf_exit_code"
