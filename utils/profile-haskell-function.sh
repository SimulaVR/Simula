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
output_was_set=0
parse_fields="avg_ms,max_ms"
parse_fields_was_set=0
hud_live_path="./HUD_profile_live.txt"
ghc_eventlog_enabled=0
ghc_eventlog_prefix=""
perf_enabled=0
perf_output_dir=""
perf_events="task-clock,context-switches,cpu-migrations,page-faults,major-faults,cycles,instructions,sched:sched_switch"
perf_mode="auto"
perf_summary=1
perf_sched_smoke=0
perf_sched_clockid="CLOCK_MONOTONIC"
perf_sched_clockid_was_set=0
perf_decode_window_padding_ms="5"
perf_full_timehist=0
perf_child=0
perf_default_parse_fields="avg_ms,max_ms,max_frame_id,max_start_mono_ns,max_end_mono_ns,max_elapsed_ns,max_elapsed_ms,max_os_tid,max_exit_os_tid,max_thread_label,max_frame_ms,max_path"

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
            output_was_set=1
            shift 2
            ;;
        --parse)
            parse_fields="${2:?profile-haskell-function.sh: --parse requires a value}"
            parse_fields_was_set=1
            shift 2
            ;;
        --hud-live-path)
            hud_live_path="${2:?profile-haskell-function.sh: --hud-live-path requires a value}"
            shift 2
            ;;
        --ghc-eventlog|--ghc-events)
            ghc_eventlog_enabled=1
            shift
            ;;
        --ghc-eventlog-prefix)
            ghc_eventlog_enabled=1
            ghc_eventlog_prefix="${2:?profile-haskell-function.sh: --ghc-eventlog-prefix requires a value}"
            shift 2
            ;;
        --perf)
            perf_enabled=1
            shift
            ;;
        --perf-output-dir|--perf-run-dir)
            perf_output_dir="${2:?profile-haskell-function.sh: $1 requires a value}"
            shift 2
            ;;
        --perf-events)
            perf_events="${2:?profile-haskell-function.sh: --perf-events requires a value}"
            shift 2
            ;;
        --perf-mode)
            perf_mode="${2:?profile-haskell-function.sh: --perf-mode requires a value}"
            shift 2
            ;;
        --perf-sched)
            perf_enabled=1
            perf_mode="sched"
            shift
            ;;
        --perf-stat-only)
            perf_enabled=1
            perf_mode="stat"
            shift
            ;;
        --perf-sched-smoke)
            perf_sched_smoke=1
            shift
            ;;
        --perf-sched-clockid)
            perf_sched_clockid="${2:?profile-haskell-function.sh: --perf-sched-clockid requires a value}"
            perf_sched_clockid_was_set=1
            shift 2
            ;;
        --perf-no-sched-clockid)
            perf_sched_clockid=""
            perf_sched_clockid_was_set=1
            shift
            ;;
        --perf-no-summary)
            perf_summary=0
            shift
            ;;
        --perf-full-timehist)
            perf_full_timehist=1
            shift
            ;;
        --perf-decode-window-padding-ms)
            perf_decode_window_padding_ms="${2:?profile-haskell-function.sh: --perf-decode-window-padding-ms requires a value}"
            shift 2
            ;;
        --_profile-child)
            perf_child=1
            shift
            ;;
        --help|-h)
            cat <<'EOF'
profile-haskell-function.sh [FLAGS] [COMMAND...]

Flags:
  --test-time TIME       How long to let Simula run before timeout. Default: 10s
  --function NAME        Value passed to SIMULA_DEBUG_PROFILE_ROOT. Required.
                         If SIMULA_HS_PROFILE_PREFIX is set and
                         SIMULA_DEBUG_PROFILE_EVENTLOG_SCOPE is unset, NAME is
                         also used as the eventlog marker scope.
  --test-setup TEXT      Free-form note describing the test setup.
  --output FILE          CSV file to append. Default: <function>-profile.csv
  --parse FIELDS         Comma/space-separated HUD_profile_live.txt fields.
                         Default: avg_ms,max_ms
  --hud-live-path FILE   Live HUD file to parse. Default: ./HUD_profile_live.txt
  --ghc-eventlog         Emit GHC RTS eventlog/stats into the run artifact dir.
                         Alias: --ghc-events
  --perf                 Run the HUD capture and attach perf to the Godot PID.
  --perf-output-dir DIR  Directory for perf/HUD artifacts. Default:
                         ./perf-runs/<utc timestamp>-<function>
  --perf-events EVENTS   perf stat event list. Default:
                         task-clock,context-switches,cpu-migrations,page-faults,major-faults,cycles,instructions,sched:sched_switch
  --perf-mode MODE       auto, stat, or sched. Default: auto
  --perf-sched           Alias for --perf --perf-mode sched.
  --perf-stat-only       Alias for --perf --perf-mode stat.
  --perf-sched-smoke     Save a minimal perf sched record/decode smoke test.
  --perf-sched-clockid CLOCKID
                         Clock used for perf sched events. Default:
                         CLOCK_MONOTONIC
  --perf-no-sched-clockid
                         Do not pass a perf sched --clockid/-k option.
  --perf-full-timehist   Decode the full Godot perf sched timehist. This can be large.
  --perf-no-summary      Do not write summary.md.
  --perf-decode-window-padding-ms MS
                         Padding around max_start/end_mono_ns for windowed timehist.
                         Default: 5

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

safe_function="$(printf '%s' "$profile_function" | tr -c '[:alnum:]_.-' '_')"
if [[ -z "$output" ]]; then
    output="${safe_function}-profile.csv"
fi

command=("$@")
if [[ ${#command[@]} -eq 0 ]]; then
    command=(./result/bin/simula --local)
fi

case "$perf_mode" in
    auto|stat|sched)
        ;;
    *)
        echo "profile-haskell-function.sh: --perf-mode must be auto, stat, or sched" >&2
        exit 2
        ;;
esac

if [[ "$perf_enabled" -eq 1 && "$parse_fields_was_set" -eq 0 ]]; then
    parse_fields="$perf_default_parse_fields"
fi

default_run_dir() {
    local timestamp_slug
    timestamp_slug="$(date -u +"%Y%m%dT%H%M%SZ")"
    printf './perf-runs/%s-%s\n' "$timestamp_slug" "$safe_function"
}

test_time_seconds() {
    local value="$1"
    awk -v value="$value" '
        BEGIN {
            if (value ~ /^[0-9]+([.][0-9]+)?$/) {
                print value
                exit 0
            }
            if (value ~ /^[0-9]+([.][0-9]+)?[smhd]$/) {
                number = substr(value, 1, length(value) - 1) + 0
                unit = substr(value, length(value), 1)
                if (unit == "s") {
                    print number
                } else if (unit == "m") {
                    print number * 60
                } else if (unit == "h") {
                    print number * 3600
                } else if (unit == "d") {
                    print number * 86400
                }
                exit 0
            }
            exit 1
        }
    '
}

profile_window_seconds="$(test_time_seconds "$test_time" || true)"
if [[ -n "$profile_window_seconds" ]]; then
    : "${SIMULA_DEBUG_PROFILE_HUD_WINDOW_S:=$profile_window_seconds}"
    if [[ -z "${SIMULA_DEBUG_PROFILE_HUD_MAX_RETAINED_FRAMES:-}" ]]; then
        SIMULA_DEBUG_PROFILE_HUD_MAX_RETAINED_FRAMES="$(
            awk -v seconds="$profile_window_seconds" 'BEGIN { frames = int(seconds * 240 + 0.999); if (frames < 512) frames = 512; print frames }'
        )"
    fi
    export SIMULA_DEBUG_PROFILE_HUD_WINDOW_S
    export SIMULA_DEBUG_PROFILE_HUD_MAX_RETAINED_FRAMES
fi

if [[ "$ghc_eventlog_enabled" -eq 1 && ! ( "$perf_enabled" -eq 1 && "$perf_child" -eq 0 ) ]]; then
    if [[ -z "$ghc_eventlog_prefix" ]]; then
        if [[ -z "$perf_output_dir" ]]; then
            ghc_eventlog_prefix="$(default_run_dir)/godot-haskell-plugin"
        else
            ghc_eventlog_prefix="$perf_output_dir/godot-haskell-plugin"
        fi
    fi
    mkdir -p "$(dirname "$ghc_eventlog_prefix")"
    export SIMULA_HS_PROFILE_PREFIX="$ghc_eventlog_prefix"
fi

if [[ -n "${SIMULA_HS_PROFILE_PREFIX:-}" && -z "${SIMULA_DEBUG_PROFILE_EVENTLOG_SCOPE+x}" ]]; then
    export SIMULA_DEBUG_PROFILE_EVENTLOG_SCOPE="$profile_function"
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

matching_local_godot_pids() {
    local repo_cwd pid cwd comm exe_base
    repo_cwd="$(pwd -P)"
    pgrep -f 'godot\.x11\.tools\.64' |
        while read -r pid; do
            [[ "$pid" == "$$" ]] && continue
            cwd="$(readlink -f "/proc/$pid/cwd" 2>/dev/null)" || continue
            [[ "$cwd" == "$repo_cwd" ]] || continue
            comm="$(cat "/proc/$pid/comm" 2>/dev/null || true)"
            exe_base="$(basename "$(readlink -f "/proc/$pid/exe" 2>/dev/null)" 2>/dev/null || true)"
            [[ "$comm" == "godot.x11.tools" || "$comm" == "godot.x11.tools.64" || "$exe_base" == "godot.x11.tools.64" ]] || continue
            printf '%s\n' "$pid"
        done
}

quote_words() {
    printf '%q ' "$@"
}

csv_field() {
    local csv_file="$1"
    local field_name="$2"
    [[ -s "$csv_file" ]] || return 0
    awk -v wanted="$field_name" '
        function csvsplit(line, out,    i, c, n, in_quote, next_c, field) {
            n = 1
            field = ""
            in_quote = 0
            for (i = 1; i <= length(line); i++) {
                c = substr(line, i, 1)
                if (in_quote) {
                    if (c == "\"") {
                        next_c = substr(line, i + 1, 1)
                        if (next_c == "\"") {
                            field = field "\""
                            i++
                        } else {
                            in_quote = 0
                        }
                    } else {
                        field = field c
                    }
                } else if (c == "\"") {
                    in_quote = 1
                } else if (c == ",") {
                    out[n++] = field
                    field = ""
                } else {
                    field = field c
                }
            }
            out[n] = field
            return n
        }
        NR == 1 {
            count = csvsplit($0, header)
            for (i = 1; i <= count; i++) {
                index_by_name[header[i]] = i
            }
            next
        }
        NR == 2 {
            count = csvsplit($0, row)
            if (wanted in index_by_name) {
                print row[index_by_name[wanted]]
            }
            exit
        }
    ' "$csv_file"
}

perf_tracefs_ready() {
    local diagnostic_file="$1"
    local path
    local -a missing=()

    for path in \
        /sys/kernel/tracing/printk_formats \
        /sys/kernel/tracing/saved_cmdlines \
        /sys/kernel/tracing/events/sched/sched_switch/format
    do
        [[ -r "$path" ]] || missing+=("$path")
    done

    if [[ "${#missing[@]}" -ne 0 ]]; then
        {
            printf 'perf sched metadata is not readable.\n'
            printf 'Missing/read-blocked files:\n'
            printf '  %s\n' "${missing[@]}"
            printf '\nFix, if appropriate for this machine:\n'
            printf '  sudo chmod a+r /sys/kernel/tracing/printk_formats /sys/kernel/tracing/saved_cmdlines\n'
        } > "$diagnostic_file"
        return 1
    fi

    return 0
}

run_perf_sched_smoke_test() {
    local smoke_txt="$1"
    local smoke_data="$2"
    local clockid="${3:-}"
    local record_status script_status latency_status
    local -a clock_args=()

    if [[ -n "$clockid" ]]; then
        clock_args=(-k "$clockid")
    fi

    {
        printf 'Minimal scheduler trace decode smoke test.\n'
        printf 'perf_sched_clockid=%s\n' "${clockid:-default}"
        printf 'Command: '
        quote_words perf sched record "${clock_args[@]}" -o "$smoke_data" -- sleep 0.2
        printf '\n\n'
    } > "$smoke_txt"

    perf sched record "${clock_args[@]}" -o "$smoke_data" -- sleep 0.2 >> "$smoke_txt" 2>&1
    record_status=$?

    {
        printf '\nperf sched record exit_code=%s\n\n' "$record_status"
        printf 'Command: '
        quote_words perf script -i "$smoke_data"
        printf '\n\n'
    } >> "$smoke_txt"
    perf script -i "$smoke_data" >> "$smoke_txt" 2>&1
    script_status=$?

    {
        printf '\nperf script exit_code=%s\n\n' "$script_status"
        printf 'Command: '
        quote_words perf sched latency -i "$smoke_data"
        printf '\n\n'
    } >> "$smoke_txt"
    perf sched latency -i "$smoke_data" >> "$smoke_txt" 2>&1
    latency_status=$?
    printf '\nperf sched latency exit_code=%s\n' "$latency_status" >> "$smoke_txt"

    [[ "$record_status" -eq 0 && "$script_status" -eq 0 && "$latency_status" -eq 0 ]]
}

clock_monotonic_seconds() {
    python3 -c 'import time; print(f"{time.clock_gettime(time.CLOCK_MONOTONIC):.9f}")' 2>/dev/null ||
        perl -MTime::HiRes=clock_gettime,CLOCK_MONOTONIC -e 'printf "%.9f\n", clock_gettime(CLOCK_MONOTONIC)' 2>/dev/null
}

write_perf_clock_sync() {
    local clock_sync_txt="$1"
    local clockid="$2"
    local mono_s uptime_s mono_to_uptime_offset

    mono_s="$(clock_monotonic_seconds || true)"
    uptime_s="$(awk '{print $1}' /proc/uptime)"
    mono_to_uptime_offset="$(
        awk -v uptime="$uptime_s" -v mono="$mono_s" '
            BEGIN {
                if (uptime != "" && mono != "") {
                    printf "%.9f", uptime - mono
                }
            }
        '
    )"

    {
        printf 'perf_sched_start_utc=%s\n' "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
        printf 'perf_sched_clockid_used=%s\n' "${clockid:-default}"
        printf 'perf_sched_start_clock_monotonic_s=%s\n' "$mono_s"
        printf 'perf_sched_start_uptime_s=%s\n' "$uptime_s"
        printf 'haskell_mono_to_uptime_offset_s=%s\n' "$mono_to_uptime_offset"
    } > "$clock_sync_txt"
}

copy_hud_artifacts_to_perf_dir() {
    local run_dir="$1"
    local hud_path

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
}

copy_profile_row_to_perf_dir() {
    local profile_csv="$1"
    local output_abs profile_abs

    if [[ ! -s "$output" ]]; then
        return 0
    fi

    output_abs="$(readlink -f "$output" 2>/dev/null || printf '%s' "$output")"
    profile_abs="$(readlink -f "$profile_csv" 2>/dev/null || printf '%s' "$profile_csv")"
    if [[ "$output_abs" == "$profile_abs" ]]; then
        return 0
    fi

    {
        head -n 1 "$output"
        tail -n 1 "$output"
    } > "$profile_csv"
}

extract_perf_value() {
    local perf_stat_file="$1"
    local metric="$2"
    [[ -s "$perf_stat_file" ]] || return 0
    awk -v metric="$metric" '
        $2 == metric || $3 == metric {
            value = $1
            gsub(/,/, "", value)
            print value
            exit
        }
    ' "$perf_stat_file"
}

extract_perf_elapsed_seconds() {
    local perf_stat_file="$1"
    [[ -s "$perf_stat_file" ]] || return 0
    awk '
        $2 == "seconds" && $3 == "time" && $4 == "elapsed" {
            value = $1
            gsub(/,/, "", value)
            print value
            exit
        }
    ' "$perf_stat_file"
}

wait_for_new_godot_pid() {
    local profile_pid="$1"
    local attempt pid
    local -a current_godot_pids=()

    for attempt in {1..300}; do
        mapfile -t current_godot_pids < <(matching_local_godot_pids)
        for pid in "${current_godot_pids[@]}"; do
            if ! pid_in_list "$pid" "${preexisting_godot_pids[@]}"; then
                printf '%s\n' "$pid"
                return 0
            fi
        done

        if ! kill -0 "$profile_pid" 2>/dev/null; then
            return 1
        fi

        sleep 0.1
    done

    return 1
}

write_perf_environment() {
    local environment_txt="$1"
    local timestamp_iso="$2"
    local repo_dir="$3"

    {
        printf 'timestamp_utc=%s\n' "$timestamp_iso"
        printf 'repo_dir=%s\n' "$repo_dir"
        printf 'perf_path=%s\n' "$(command -v perf 2>/dev/null || true)"
        if command -v perf >/dev/null 2>&1; then
            printf 'perf_version=%s\n' "$(perf --version 2>&1)"
        fi
        printf 'uname=%s\n' "$(uname -a)"
        if [[ -r /proc/sys/kernel/perf_event_paranoid ]]; then
            printf 'perf_event_paranoid=%s\n' "$(cat /proc/sys/kernel/perf_event_paranoid)"
        fi
        if [[ -r /proc/sys/kernel/kptr_restrict ]]; then
            printf 'kptr_restrict=%s\n' "$(cat /proc/sys/kernel/kptr_restrict)"
        fi
        if [[ -r /sys/kernel/tracing/printk_formats ]]; then
            printf 'tracefs_printk_formats=readable\n'
        else
            printf 'tracefs_printk_formats=not_readable\n'
        fi
        if [[ -r /sys/kernel/tracing/saved_cmdlines ]]; then
            printf 'tracefs_saved_cmdlines=readable\n'
        else
            printf 'tracefs_saved_cmdlines=not_readable\n'
        fi
    } > "$environment_txt"
}

decode_perf_sched_outputs() {
    local run_dir="$1"
    local sched_data="$2"
    local godot_pid="$3"
    local profile_csv="$4"
    local decode_status_txt="$5"
    local clock_sync_txt="$6"
    local latency_txt="$run_dir/sched-latency.txt"
    local latency_err="$run_dir/sched-latency.err"
    local timehist_window_txt="$run_dir/sched-timehist-max-window.txt"
    local timehist_window_err="$run_dir/sched-timehist-max-window.err"
    local timehist_full_txt="$run_dir/sched-timehist-godot.txt"
    local timehist_full_err="$run_dir/sched-timehist-godot.err"
    local max_start_mono_ns max_end_mono_ns max_os_tid max_path
    local start_s end_s time_arg first_perf_time perf_start_uptime perf_start_mono perf_sched_clockid_used
    local haskell_mono_to_uptime_offset perf_trace_to_uptime_offset perf_clock_offset clock_sync_strategy
    local latency_status window_status full_status

    perf sched latency -i "$sched_data" > "$latency_txt" 2> "$latency_err"
    latency_status=$?

    max_start_mono_ns="$(csv_field "$profile_csv" max_start_mono_ns)"
    max_end_mono_ns="$(csv_field "$profile_csv" max_end_mono_ns)"
    max_os_tid="$(csv_field "$profile_csv" max_os_tid)"
    max_path="$(csv_field "$profile_csv" max_path)"

    window_status="skipped"
    if [[ -n "$max_start_mono_ns" && -n "$max_end_mono_ns" ]]; then
        first_perf_time="$(
            perf script -i "$sched_data" 2>/dev/null |
                awk '
                    match($0, /[0-9]+[.][0-9]+:/) {
                        value = substr($0, RSTART, RLENGTH - 1)
                        print value
                        exit
                    }
                '
        )"
        perf_start_uptime="$(
            awk -F= '$1 == "perf_sched_start_uptime_s" { print $2; exit }' "$clock_sync_txt" 2>/dev/null
        )"
        perf_start_mono="$(
            awk -F= '$1 == "perf_sched_start_clock_monotonic_s" { print $2; exit }' "$clock_sync_txt" 2>/dev/null
        )"
        perf_sched_clockid_used="$(
            awk -F= '$1 == "perf_sched_clockid_used" { print $2; exit }' "$clock_sync_txt" 2>/dev/null
        )"
        haskell_mono_to_uptime_offset="$(
            awk -F= '$1 == "haskell_mono_to_uptime_offset_s" { print $2; exit }' "$clock_sync_txt" 2>/dev/null
        )"
        if [[ "$perf_sched_clockid_used" == "CLOCK_MONOTONIC" ]]; then
            clock_sync_strategy="perf_CLOCK_MONOTONIC"
            perf_trace_to_uptime_offset=""
            perf_clock_offset="0.000000000"
        else
            clock_sync_strategy="uptime_bridge"
            if [[ -z "$haskell_mono_to_uptime_offset" && -n "$perf_start_mono" && -n "$perf_start_uptime" ]]; then
                haskell_mono_to_uptime_offset="$(
                    awk -v uptime="$perf_start_uptime" -v mono="$perf_start_mono" '
                        BEGIN {
                            if (uptime != "" && mono != "") {
                                printf "%.9f", uptime - mono
                            }
                        }
                    '
                )"
            fi
            perf_trace_to_uptime_offset="$(
                awk -v perf_time="$first_perf_time" -v uptime="$perf_start_uptime" '
                    BEGIN {
                        if (perf_time != "" && uptime != "") {
                            printf "%.9f", perf_time - uptime
                        }
                    }
                '
            )"
            perf_clock_offset="$(
                awk -v mono_to_uptime="$haskell_mono_to_uptime_offset" -v perf_to_uptime="$perf_trace_to_uptime_offset" '
                    BEGIN {
                        if (mono_to_uptime != "" && perf_to_uptime != "") {
                            printf "%.9f", mono_to_uptime + perf_to_uptime
                        } else if (perf_to_uptime != "") {
                            printf "%.9f", perf_to_uptime
                        }
                    }
                '
            )"
        fi
        start_s="$(
            awk -v ns="$max_start_mono_ns" -v pad_ms="$perf_decode_window_padding_ms" -v offset="$perf_clock_offset" \
                'BEGIN { value = ns / 1000000000.0 + offset - pad_ms / 1000.0; if (value < 0) value = 0; printf "%.9f", value }'
        )"
        end_s="$(
            awk -v ns="$max_end_mono_ns" -v pad_ms="$perf_decode_window_padding_ms" -v offset="$perf_clock_offset" \
                'BEGIN { printf "%.9f", ns / 1000000000.0 + offset + pad_ms / 1000.0 }'
        )"
        time_arg="${start_s},${end_s}"
        {
            printf 'max_path=%s\n' "$max_path"
            printf 'max_os_tid=%s\n' "$max_os_tid"
            printf 'max_start_mono_ns=%s\n' "$max_start_mono_ns"
            printf 'max_end_mono_ns=%s\n' "$max_end_mono_ns"
            printf 'first_perf_time=%s\n' "$first_perf_time"
            printf 'perf_sched_clockid_used=%s\n' "$perf_sched_clockid_used"
            printf 'clock_sync_strategy=%s\n' "$clock_sync_strategy"
            printf 'perf_sched_start_clock_monotonic_s=%s\n' "$perf_start_mono"
            printf 'perf_sched_start_uptime_s=%s\n' "$perf_start_uptime"
            printf 'haskell_mono_to_uptime_offset_s=%s\n' "$haskell_mono_to_uptime_offset"
            printf 'perf_trace_to_uptime_offset_s=%s\n' "$perf_trace_to_uptime_offset"
            printf 'perf_clock_offset_s=%s\n' "$perf_clock_offset"
            printf 'perf_time_arg=%s\n\n' "$time_arg"
        } > "$timehist_window_txt"
        perf sched timehist -i "$sched_data" -p "$godot_pid" --time "$time_arg" >> "$timehist_window_txt" 2> "$timehist_window_err"
        window_status=$?
    else
        {
            printf 'No max_start_mono_ns/max_end_mono_ns fields were found in %s.\n' "$profile_csv"
            printf 'Compile the HUD metadata patch, or pass --parse with the max_* fields, then rerun for windowed timehist.\n'
        } > "$timehist_window_txt"
    fi

    full_status="skipped"
    if [[ "$perf_full_timehist" -eq 1 ]]; then
        perf sched timehist -i "$sched_data" -p "$godot_pid" > "$timehist_full_txt" 2> "$timehist_full_err"
        full_status=$?
    fi

    {
        printf 'latency_status=%s\n' "$latency_status"
        printf 'timehist_window_status=%s\n' "$window_status"
        printf 'timehist_full_status=%s\n' "$full_status"
    } > "$decode_status_txt"
}

write_perf_summary() {
    local summary_md="$1"
    local run_dir="$2"
    local timestamp_iso="$3"
    local profile_pid="$4"
    local godot_pid="$5"
    local profile_status="$6"
    local perf_stat_status="$7"
    local perf_sched_status="$8"
    local sched_decode_status_txt="$9"
    local profile_csv="${10}"
    local perf_stat_txt="${11}"
    local sched_data="${12}"
    local task_clock_ms context_switches cpu_migrations page_faults major_faults cycles instructions sched_switches elapsed_s
    local max_frame_id max_path max_os_tid max_start_mono_ns max_end_mono_ns max_elapsed_ms max_frame_ms

    task_clock_ms="$(extract_perf_value "$perf_stat_txt" task-clock)"
    context_switches="$(extract_perf_value "$perf_stat_txt" context-switches)"
    cpu_migrations="$(extract_perf_value "$perf_stat_txt" cpu-migrations)"
    page_faults="$(extract_perf_value "$perf_stat_txt" page-faults)"
    major_faults="$(extract_perf_value "$perf_stat_txt" major-faults)"
    cycles="$(extract_perf_value "$perf_stat_txt" cycles)"
    instructions="$(extract_perf_value "$perf_stat_txt" instructions)"
    sched_switches="$(extract_perf_value "$perf_stat_txt" sched:sched_switch)"
    elapsed_s="$(extract_perf_elapsed_seconds "$perf_stat_txt")"

    max_frame_id="$(csv_field "$profile_csv" max_frame_id)"
    max_path="$(csv_field "$profile_csv" max_path)"
    max_os_tid="$(csv_field "$profile_csv" max_os_tid)"
    max_start_mono_ns="$(csv_field "$profile_csv" max_start_mono_ns)"
    max_end_mono_ns="$(csv_field "$profile_csv" max_end_mono_ns)"
    max_elapsed_ms="$(csv_field "$profile_csv" max_elapsed_ms)"
    max_frame_ms="$(csv_field "$profile_csv" max_frame_ms)"

    {
        printf '# perf spike triangulation\n\n'
        printf 'Run directory: `%s`\n\n' "$run_dir"
        printf 'Timestamp UTC: `%s`\n\n' "$timestamp_iso"
        printf 'Function: `%s`\n\n' "$profile_function"
        printf 'Test setup: `%s`\n\n' "$test_setup"
        printf '| Item | Value |\n'
        printf '| --- | ---: |\n'
        printf '| profile wrapper PID | %s |\n' "$profile_pid"
        printf '| Godot PID | %s |\n' "${godot_pid:-n/a}"
        printf '| profile exit code | %s |\n' "$profile_status"
        printf '| perf stat exit code | %s |\n' "$perf_stat_status"
        printf '| perf sched record exit code | %s |\n\n' "$perf_sched_status"

        printf '## Max HUD call\n\n'
        printf '| Field | Value |\n'
        printf '| --- | --- |\n'
        printf '| max_path | `%s` |\n' "${max_path:-n/a}"
        printf '| max_frame_id | %s |\n' "${max_frame_id:-n/a}"
        printf '| max_frame_ms | %s |\n' "${max_frame_ms:-n/a}"
        printf '| max_elapsed_ms | %s |\n' "${max_elapsed_ms:-n/a}"
        printf '| max_os_tid | %s |\n' "${max_os_tid:-n/a}"
        printf '| max_start_mono_ns | %s |\n' "${max_start_mono_ns:-n/a}"
        printf '| max_end_mono_ns | %s |\n\n' "${max_end_mono_ns:-n/a}"

        printf '## Coarse perf counters\n\n'
        printf '| Metric | Value |\n'
        printf '| --- | ---: |\n'
        printf '| elapsed wall time | %s s |\n' "${elapsed_s:-n/a}"
        printf '| task-clock | %s ms |\n' "${task_clock_ms:-n/a}"
        printf '| context switches | %s |\n' "${context_switches:-n/a}"
        printf '| sched:sched_switch events | %s |\n' "${sched_switches:-n/a}"
        printf '| CPU migrations | %s |\n' "${cpu_migrations:-n/a}"
        printf '| page faults | %s |\n' "${page_faults:-n/a}"
        printf '| major faults | %s |\n' "${major_faults:-n/a}"
        printf '| cycles | %s |\n' "${cycles:-n/a}"
        printf '| instructions | %s |\n\n' "${instructions:-n/a}"

        printf '## Scheduler decode\n\n'
        if [[ -s "$sched_decode_status_txt" ]]; then
            printf '```text\n'
            cat "$sched_decode_status_txt"
            printf '```\n\n'
        else
            printf 'Scheduler decode did not run.\n\n'
        fi

        if [[ -s "$run_dir/perf-clock-sync.txt" ]]; then
            printf '## Clock sync\n\n'
            printf '```text\n'
            cat "$run_dir/perf-clock-sync.txt"
            printf '```\n\n'
        fi

        printf '## HUD profile row\n\n'
        if [[ -s "$profile_csv" ]]; then
            printf '```csv\n'
            cat "$profile_csv"
            printf '```\n\n'
        else
            printf 'No run-local profile.csv was written.\n\n'
        fi

        printf '## Files\n\n'
        find "$run_dir" -maxdepth 1 -type f -printf '- `%f`\n' | sort

        if [[ -n "$sched_data" && -e "$sched_data" ]]; then
            printf '\nScheduler data: `%s`\n' "$(basename "$sched_data")"
        fi
    } > "$summary_md"
}

run_perf_parent() {
    local script_path repo_dir timestamp_slug timestamp_iso run_dir
    local profile_log profile_csv perf_stat_txt environment_txt command_txt summary_md
    local sched_data sched_record_log sched_preflight_txt sched_smoke_txt sched_smoke_data sched_decode_status_txt
    local process_snapshot_txt status_txt artifact_sizes_txt clock_sync_txt
    local profile_pid godot_pid profile_status perf_stat_status perf_sched_status
    local perf_stat_pid perf_sched_pid preflight_sched_ready should_run_sched
    local selected_perf_sched_clockid="$perf_sched_clockid"
    local child_parse_fields="$parse_fields"
    local child_output
    local -a child_args=()
    local -a sched_clock_args=()

    if ! command -v perf >/dev/null 2>&1; then
        echo "profile-haskell-function.sh: --perf requested but perf is not on PATH" >&2
        exit 127
    fi

    script_path="$(readlink -f "${BASH_SOURCE[0]}")"
    repo_dir="$(pwd -P)"
    timestamp_slug="$(date -u +"%Y%m%dT%H%M%SZ")"
    timestamp_iso="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

    if [[ -z "$perf_output_dir" ]]; then
        run_dir="./perf-runs/${timestamp_slug}-${safe_function}"
    else
        run_dir="$perf_output_dir"
    fi

    mkdir -p "$run_dir"
    if [[ "$ghc_eventlog_enabled" -eq 1 && -z "$ghc_eventlog_prefix" ]]; then
        ghc_eventlog_prefix="$run_dir/godot-haskell-plugin"
    fi

    profile_log="$run_dir/profile-command.log"
    profile_csv="$run_dir/profile.csv"
    perf_stat_txt="$run_dir/perf-stat.txt"
    environment_txt="$run_dir/environment.txt"
    command_txt="$run_dir/command.txt"
    summary_md="$run_dir/summary.md"
    sched_data="$run_dir/sched.data"
    sched_record_log="$run_dir/perf-sched-record.log"
    sched_preflight_txt="$run_dir/perf-sched-preflight.txt"
    sched_smoke_txt="$run_dir/perf-sched-smoke.txt"
    sched_smoke_data="$run_dir/perf-sched-smoke.data"
    sched_decode_status_txt="$run_dir/sched-decode.status"
    process_snapshot_txt="$run_dir/process-snapshot.txt"
    status_txt="$run_dir/profile.status"
    artifact_sizes_txt="$run_dir/artifact-sizes.txt"
    clock_sync_txt="$run_dir/perf-clock-sync.txt"
    child_output="$output"
    if [[ "$output_was_set" -eq 0 ]]; then
        child_output="$profile_csv"
    fi

    write_perf_environment "$environment_txt" "$timestamp_iso" "$repo_dir"

    child_args=(
        --_profile-child
        --test-time "$test_time"
        --function "$profile_function"
        --test-setup "$test_setup"
        --output "$child_output"
        --parse "$child_parse_fields"
        --hud-live-path "$hud_live_path"
        --
        "${command[@]}"
    )
    if [[ "$ghc_eventlog_enabled" -eq 1 ]]; then
        child_args=(
            --ghc-eventlog-prefix "$ghc_eventlog_prefix"
            "${child_args[@]}"
        )
    fi

    {
        printf 'Profile command:\n'
        quote_words "$script_path" "${child_args[@]}"
        printf '\n\n'
        printf 'perf stat command, after Godot PID discovery:\n'
        quote_words perf stat -p '<godot-pid>' -o "$perf_stat_txt" -e "$perf_events" -- sleep "$test_time"
        printf '\n\n'
        printf 'perf sched command, when enabled and preflight passes:\n'
        if [[ -n "$perf_sched_clockid" ]]; then
            sched_clock_args=(-k "$perf_sched_clockid")
        else
            sched_clock_args=()
        fi
        quote_words perf sched record "${sched_clock_args[@]}" -p '<godot-pid>' -o "$sched_data" -- sleep "$test_time"
        printf '\n'
    } > "$command_txt"

    preflight_sched_ready=0
    if [[ "$perf_mode" == "sched" || "$perf_mode" == "auto" ]]; then
        if perf_tracefs_ready "$sched_preflight_txt"; then
            if run_perf_sched_smoke_test "$sched_smoke_txt" "$sched_smoke_data" "$perf_sched_clockid"; then
                preflight_sched_ready=1
                selected_perf_sched_clockid="$perf_sched_clockid"
            elif [[ -n "$perf_sched_clockid" && "$perf_sched_clockid_was_set" -eq 0 ]] && run_perf_sched_smoke_test "$sched_smoke_txt" "$sched_smoke_data" ""; then
                preflight_sched_ready=1
                selected_perf_sched_clockid=""
                printf 'perf sched smoke test failed with clockid %s; fell back to perf default clock.\n' "$perf_sched_clockid" >> "$sched_preflight_txt"
            else
                printf 'perf sched smoke test failed; not recording main sched trace.\n' >> "$sched_preflight_txt"
            fi
        fi
    elif [[ "$perf_sched_smoke" -eq 1 ]]; then
        run_perf_sched_smoke_test "$sched_smoke_txt" "$sched_smoke_data" "$perf_sched_clockid" || true
    fi

    mapfile -t preexisting_godot_pids < <(matching_local_godot_pids)

    "$script_path" "${child_args[@]}" > "$profile_log" 2>&1 &
    profile_pid=$!

    godot_pid="$(wait_for_new_godot_pid "$profile_pid" || true)"
    if [[ -n "$godot_pid" ]]; then
        printf '%s\n' "$godot_pid" > "$run_dir/godot.pid"
        ps -L -p "$godot_pid" -o pid,tid,comm,stat,psr,pcpu,pmem,wchan:30,args > "$process_snapshot_txt" 2>&1 || true

        perf stat -p "$godot_pid" -o "$perf_stat_txt" -e "$perf_events" -- sleep "$test_time" > "$run_dir/perf-stat-command.log" 2>&1 &
        perf_stat_pid=$!

        should_run_sched=0
        if [[ "$preflight_sched_ready" -eq 1 ]]; then
            should_run_sched=1
        fi

        if [[ "$should_run_sched" -eq 1 ]]; then
            if [[ -n "$selected_perf_sched_clockid" ]]; then
                sched_clock_args=(-k "$selected_perf_sched_clockid")
            else
                sched_clock_args=()
            fi
            write_perf_clock_sync "$clock_sync_txt" "$selected_perf_sched_clockid"
            perf sched record "${sched_clock_args[@]}" -p "$godot_pid" -o "$sched_data" -- sleep "$test_time" > "$sched_record_log" 2>&1 &
            perf_sched_pid=$!
        else
            perf_sched_pid=""
            perf_sched_status="skipped"
        fi
    else
        printf 'Could not discover a new local godot.x11.tools.64 PID before the profile command exited or timed out.\n' > "$process_snapshot_txt"
        printf 'perf stat and perf sched were not started.\n' > "$perf_stat_txt"
        perf_stat_pid=""
        perf_sched_pid=""
        perf_stat_status="skipped"
        perf_sched_status="skipped"
    fi

    wait "$profile_pid"
    profile_status=$?

    if [[ -n "${perf_stat_pid:-}" ]]; then
        wait "$perf_stat_pid"
        perf_stat_status=$?
    fi

    if [[ -n "${perf_sched_pid:-}" ]]; then
        wait "$perf_sched_pid"
        perf_sched_status=$?
    fi

    copy_hud_artifacts_to_perf_dir "$run_dir"
    if [[ "$child_output" != "$profile_csv" ]]; then
        copy_profile_row_to_perf_dir "$profile_csv"
    fi

    if [[ -n "$godot_pid" && -s "$sched_data" && "${perf_sched_status:-skipped}" == "0" ]]; then
        decode_perf_sched_outputs "$run_dir" "$sched_data" "$godot_pid" "$profile_csv" "$sched_decode_status_txt" "$clock_sync_txt"
    fi

    {
        printf 'profile_pid=%s\n' "$profile_pid"
        printf 'godot_pid=%s\n' "${godot_pid:-}"
        printf 'started_utc=%s\n' "$timestamp_iso"
        printf 'profile_status=%s\n' "$profile_status"
        printf 'perf_stat_status=%s\n' "${perf_stat_status:-skipped}"
        printf 'perf_sched_status=%s\n' "${perf_sched_status:-skipped}"
        printf 'ended_utc=%s\n' "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
    } > "$status_txt"

    find "$run_dir" -maxdepth 1 -type f -printf '%f %s bytes\n' | sort > "$artifact_sizes_txt"

    if [[ "$perf_summary" -eq 1 ]]; then
        write_perf_summary \
            "$summary_md" \
            "$run_dir" \
            "$timestamp_iso" \
            "$profile_pid" \
            "${godot_pid:-}" \
            "$profile_status" \
            "${perf_stat_status:-skipped}" \
            "${perf_sched_status:-skipped}" \
            "$sched_decode_status_txt" \
            "$profile_csv" \
            "$perf_stat_txt" \
            "$sched_data"
    fi

    echo "profile-haskell-function.sh: perf run directory $run_dir"
    if [[ "$perf_summary" -eq 1 ]]; then
        echo "profile-haskell-function.sh: summary $summary_md"
    fi
    echo "profile-haskell-function.sh: profile exit code was $profile_status"

    return "$profile_status"
}

if [[ "$perf_enabled" -eq 1 && "$perf_child" -eq 0 ]]; then
    run_perf_parent
    exit "$?"
fi

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
        /^max_[[:alnum:]_]+: / {
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
if [[ "$ghc_eventlog_enabled" -eq 1 ]]; then
    echo "profile-haskell-function.sh: GHC eventlog prefix $SIMULA_HS_PROFILE_PREFIX"
fi
echo "profile-haskell-function.sh: simula exit code was $exit_code"
