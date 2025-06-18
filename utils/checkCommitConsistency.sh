GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m' # No Color

# Helper function to normalize git URLs for comparison
# Example: normalize_url "git@github.com:SimulaVR/wlroots.git"
# Output: "https://github.com/SimulaVR/wlroots"
normalize_url() {
  echo "$1" |
    sed \
      -e 's/\.git$//' \
      -e 's/^git@github\.com:/https:\/\/github.com\//' \
      -e 's/^git@gitlab\.\([^:]*\):/https:\/\/gitlab.\1\//'
}

# Function to check if commit A is ancestor of commit B in the given directory
# Example: is_ancestor "./submodules/wlroots" "abc123" "def456"
# Returns: 0 if abc123 is ancestor of def456, 1 otherwise
# Test: cd submodules/wlroots && git log --oneline | head -5  # get two commits
#       is_ancestor "./submodules/wlroots" "older_commit" "newer_commit" && echo "Is ancestor" || echo "Not ancestor"
is_ancestor() {
    local dir="$1"
    local commit_a="$2"
    local commit_b="$3"
    (cd "$dir" && git merge-base --is-ancestor "$commit_a" "$commit_b" 2>/dev/null)
}

# Example call: get_commit_date "./submodules/wlroots" "9a77919f23ca57d3dd48a50bcddc2de27b12e1"
# Output: "2025-06-16 16:57:05"
get_commit_date() {
    local dir="$1"
    local commit="$2"
    (cd "$dir" && git show -s --format=%ci "$commit" 2>/dev/null | cut -d' ' -f1-2)
}

# Example call: count_commits_between "./submodules/wlroots" "abc123" "def456"
# Output: "5" (number of commits from abc123 to def456)
count_commits_between() {
    local dir="$1"
    local from="$2"
    local to="$3"
    (cd "$dir" && git rev-list --count "$from".."$to" 2>/dev/null)
}

# Extract the commit being used for a submodule from Simula's various flake files
# Most of the `sed` calls are LLM generated (but tested).
# Example call: extract_flake_commit "wlroots"
# Output: "7d065df6f723bbb592fc50150feb213d323f37c6"
extract_flake_commit() {
    local submodule_name="$1"
    
    case "$submodule_name" in
      "gdwlroots")
          sed -nE '/gdwlroots = pkgs\.fetchFromGitHub/,/};/ {
              s/.*rev\s*=\s*"([0-9a-f]{7,64})".*/\1/p
          }' "./submodules/godot/flake.nix" | head -1
          ;;
      "wlroots")
          sed -nE '/wlroots-flake\s*=\s*\{/,/\};/ {
        s/.*rev=([a-f0-9]{7,64}).*/\1/p
    }' "./submodules/godot/flake.nix" | head -1
          ;;
        "environments")
            sed -n '/^\s*environments\s*=\s*{/,/^\s*};/{/rev=/{s/.*rev=\([a-f0-9]\+\).*/\1/p;}}' "./flake.nix"
            ;;
        "i3status")
            sed -nE '/^\s*i3status-fork\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p' "./flake.nix"
            ;;
        "godot")
            sed -nE '/^\s*godot\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p' "./flake.nix"
            ;;
        "godot-haskell")
            sed -nE '/^\s*godot-haskell\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p' "./flake.nix"
            ;;
        "godot-haskell-cabal")
            sed -nE '/^\s*godot-haskell\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p' "./flake.nix"
            ;;
        "monado")
            sed -nE '/^\s*monado\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p' "./flake.nix"
            ;;
        ,*)
            sed -nE "/^\s*${submodule_name}\.url\s*=/ s/.*rev=([a-f0-9]+).*/\1/p" "./flake.nix"
            ;;
    esac
}

# Extract the GitHub URL being used to pull Simula's submodules from our flake files
# The `sed` commands are mostly LLM generated (but tested).
# Example call: extract_flake_repo_url "wlroots"
# Output: "https://github.com/SimulaVR/wlroots"
extract_flake_repo_url() {
    local submodule_name="$1"
    
    case "$submodule_name" in
        "gdwlroots")
            local owner=$(sed -nE '/gdwlroots = pkgs\.fetchFromGitHub/,/};/ {
                /owner\s*=\s*"([^"]+)"/ s//\1/p
            }' "./submodules/godot/flake.nix" | head -1 | tr -d '[:space:];')
            local repo=$(sed -nE '/gdwlroots = pkgs\.fetchFromGitHub/,/};/ {
                /repo\s*=\s*"([^"]+)"/ s//\1/p
            }' "./submodules/godot/flake.nix" | head -1 | tr -d '[:space:];')
            if [[ -n "$owner" && -n "$repo" ]]; then
                echo "https://github.com/${owner}/${repo}"
            fi
            ;;
        "wlroots")
            sed -nE '/wlroots-flake\s*=\s*\{/,/\};/ {
                /url\s*=\s*"git\+([^?]+)/ {
                    s//\1/
                    s/\?.*$//
                    s/^[[:space:]]*//
                    s/[[:space:]]*$//
                    p
                }
            }' "./submodules/godot/flake.nix" | head -1
            ;;
        "environments")
            # Extract from git+https URL in block format
            sed -nE '/environments\s*=\s*\{/,/\};/ {
                /url\s*=/ {
                    s/.*git\+https:\/\/([^?]+).*/https:\/\/\1/
                    p
                }
            }' "./flake.nix" | head -1
            ;;
        "i3status")
            # Extract from git+https URL in inline format
            sed -nE '/^\s*i3status-fork\.url\s*=/ s/.*git\+https:\/\/([^?]+).*/https:\/\/\1/p' "./flake.nix"
            ;;
        "godot"|"godot-haskell"|"monado")
            # Generic extraction for git+https URLs in inline format
            sed -nE "/^\s*${submodule_name}\.url\s*=/ s/.*git\+https:\/\/([^?]+).*/https:\/\/\1/p" "./flake.nix"
            ;;
        "godot-haskell-cabal")
            # Same as godot-haskell
            sed -nE '/^\s*godot-haskell\.url\s*=/ s/.*git\+https:\/\/([^?]+).*/https:\/\/\1/p' "./flake.nix"
            ;;
        *)
            # ASsume git+https URLs for generic extraction
            sed -nE "/^\s*${submodule_name}(-fork)?\.url\s*=/ s/.*git\+https:\/\/([^?]+).*/https:\/\/\1/p" "./flake.nix"
            ;;
    esac
}


# Example call: display_repo_info "./submodules/wlroots" "./submodules/godot/flake.nix" \
#                                 "https://github.com/SimulaVR/wlroots" \
#                                 "https://gitlab.freedesktop.org/wlroots/wlroots"
# Output:
#   ./submodules/wlroots vs ./submodules/godot/flake.nix
#   Local upstream: https://github.com/SimulaVR/wlroots
#   Flake repo:     https://gitlab.freedesktop.org/wlroots/wlroots (different!)
display_repo_info() {
    local local_submodule_path="$1"
    local flake_file="$2"
    local local_upstream_url="$3"
    local flake_repo_url="$4"
    
    echo -e "${BOLD}${local_submodule_path} vs ${flake_file}${NC}"
    if [ -n "$local_upstream_url" ]; then
        echo -e "  ${CYAN}Local upstream: ${local_upstream_url}${NC}"
    fi
    if [ -n "$flake_repo_url" ]; then
        local normalized_upstream=$(normalize_url "$local_upstream_url")
        local normalized_flake=$(normalize_url "$flake_repo_url")
        if [ "$normalized_upstream" != "$normalized_flake" ]; then
            echo -e "  ${CYAN}Flake repo:     ${flake_repo_url}${NC} ${YELLOW}(different!)${NC}"
        else
            echo -e "  ${CYAN}Flake repo:     ${flake_repo_url}${NC}"
        fi
    fi
}

# Example call: display_commit_comparison "./submodules/wlroots" \
#                                        - local commit
#                                        "7d065df6f723bbb592fc50150feb213d323f37c6"  # <- flake commit
# Output: "↳ Local is newer (5 commits ahead of flake)" or "↳ Flake is newer (local is 3 commits behind)"
display_commit_comparison() {
    local local_submodule_path="$1"
    local local_submodule_commit="$2"
    local flake_commit="$3"
    
    if is_ancestor "$local_submodule_path" "$flake_commit" "$local_submodule_commit"; then
        local commits_ahead=$(count_commits_between "$local_submodule_path" "$flake_commit" "$local_submodule_commit")
        echo -e "  ${GREEN}↳ Local is newer (${commits_ahead} commits ahead of flake)${NC}"
    elif is_ancestor "$local_submodule_path" "$local_submodule_commit" "$flake_commit"; then
        local commits_behind=$(count_commits_between "$local_submodule_path" "$local_submodule_commit" "$flake_commit")
        echo -e "  ${YELLOW}↳ Flake is newer (local is ${commits_behind} commits behind)${NC}"
    else
        # They've diverged - check dates
        local flake_date=$(get_commit_date "$local_submodule_path" "$flake_commit")
        local local_date=$(get_commit_date "$local_submodule_path" "$local_submodule_commit")
        if [ -n "$flake_date" ] && [ -n "$local_date" ]; then
            echo -e "  ${YELLOW}↳ Commits have diverged${NC}"
            echo -e "    Flake date: $flake_date"
            echo -e "    Local date: $local_date"
        else
            echo -e "  ${YELLOW}↳ Cannot determine relationship (diverged branches?)${NC}"
        fi
    fi
}

# Example call: check_upstream_status "./submodules/wlroots" \
#                                    "9a77919f23ca57d3dd48a50bcddc2de27b12e1" \   # <- local commit
#                                    "7d065df6f723bbb592fc50150feb213d323f37c6" \ # <- flake commit
#                                    "https://github.com/SimulaVR/wlroots"
# Output:
#   Checking upstream...
#   Upstream HEAD: abc123def456...
#   ↳ Upstream is ahead of both:
#     Local: 10 commits behind
#     Flake: 15 commits behind
check_upstream_status() {
    local local_submodule_path="$1"
    local local_submodule_commit="$2"
    local local_upstream_url="$3"
    local flake_commit="$4"

    if [ -z "$local_upstream_url" ]; then
        return
    fi

    echo -e "${DIM}  Checking upstream...${NC}"

    # Show local branch info
    local local_branch=$(cd "$local_submodule_path" && git rev-parse --abbrev-ref HEAD)
    local tracking_branch=$(cd "$local_submodule_path" && git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)
    echo -e "  Local branch: $local_branch"
    [ -n "$tracking_branch" ] && echo -e "  Tracking branch: $tracking_branch"

    # Get remote default branch
    local default_branch=$(cd "$local_submodule_path" && git remote show origin | awk '/HEAD branch/ {print $NF}')
    echo -e "  Default upstream branch: $default_branch"

    local upstream_head=$(cd "$local_submodule_path" && git ls-remote "$local_upstream_url" HEAD 2>/dev/null | cut -f1)

    if [ -z "$upstream_head" ]; then
        return
    fi

    echo -e "  ${BLUE}Upstream HEAD: ${upstream_head:0:7}...${NC}"

    if is_ancestor "$local_submodule_path" "$upstream_head" "$local_submodule_commit"; then
        if [ "$local_submodule_commit" = "$upstream_head" ]; then
            echo -e "  ${GREEN}↳ Local is at upstream HEAD${NC}"
        else
            echo -e "  ${GREEN}↳ Local is ahead of upstream${NC}"
        fi
        return
    fi

    (cd "$local_submodule_path" && git fetch "$local_upstream_url" HEAD 2>/dev/null)

    local local_behind=$(count_commits_between "$local_submodule_path" "$local_submodule_commit" "$upstream_head")
    local flake_behind=$(count_commits_between "$local_submodule_path" "$flake_commit" "$upstream_head")

    if [[ "$local_submodule_commit" == "$upstream_head"* ]]; then
        echo -e "  ${GREEN}↳ Local is at upstream HEAD${NC}"
    elif [[ "$flake_commit" == "$upstream_head"* ]]; then
        echo -e "  ${GREEN}↳ Flake is at upstream HEAD${NC}"
    elif [ -n "$local_behind" ] && [ -n "$flake_behind" ]; then
        if [ "$local_behind" -eq 0 ]; then
            echo -e "  ${GREEN}↳ Local is up-to-date with upstream${NC}"
        elif [ "$flake_behind" -eq 0 ]; then
            echo -e "  ${GREEN}↳ Flake is up-to-date with upstream${NC}"
        else
            echo -e "  ${BLUE}↳ Upstream is ahead of both:${NC}"
            [ "$local_behind" -gt 0 ] && echo -e "    Local: $local_behind commits behind"
            [ "$flake_behind" -gt 0 ] && echo -e "    Flake: $flake_behind commits behind"
        fi
    else
        echo -e "  ${YELLOW}↳ Upstream may have updates${NC}"
    fi
}

# Main function to check commit consistency
# Example call: checkCommitConsistency "wlroots"
# Output: Full comparison report including repo URLs, commits, and upstream status
# Test: checkCommitConsistency "godot"
checkCommitConsistency() {
    local submodule_name="$1"

    if [ -z "$submodule_name" ]; then
        echo "Check the consistency between flake modules pulled from GitHub and local git submodules"
        echo "Usage: checkCommitConsistency <submodule_name>"
        echo "Supported submodules: gdwlroots, godot, godot-haskell, godot-haskell-cabal, environments, i3status, monado, wlroots"
        echo "Example: checkCommitConsistency godot"
        return 1
    fi

    local flake_file=""
    local local_submodule_path=""

    case "$submodule_name" in
        "godot")
            flake_file="./flake.nix"
            local_submodule_path="./submodules/godot"
            ;;
        "godot-haskell")
            flake_file="./flake.nix"
            local_submodule_path="./submodules/godot-haskell"
            ;;
        "godot-haskell-cabal")
            flake_file="./flake.nix"
            local_submodule_path="./submodules/godot-haskell-cabal"
            ;;
        "environments")
            flake_file="./flake.nix"
            local_submodule_path="./environments"
            ;;
        "i3status")
            flake_file="./flake.nix"
            local_submodule_path="./submodules/i3status"
            ;;
        "monado")
            flake_file="./flake.nix"
            local_submodule_path="./submodules/monado"
            ;;
        "gdwlroots")
            flake_file="./submodules/godot/flake.nix"
            local_submodule_path="./submodules/godot/modules/gdwlroots"
            ;;
        "wlroots")
            flake_file="./submodules/godot/flake.nix"
            local_submodule_path="./submodules/wlroots"
            ;;
        ,*)
            echo -e "${RED}Error: Unsupported submodule '${submodule_name}'${NC}"
            echo "Supported submodules: gdwlroots, godot, godot-haskell, godot-haskell-cabal, environments, i3status, monado, wlroots"
            return 1
            ;;
    esac

    # Validate flake file exists
    if [ ! -f "$flake_file" ]; then
        echo -e "${RED}Error: Flake file ${flake_file} does not exist${NC}"
        return 1
    fi

    # Validate submodule directory exists
    if [ ! -d "$local_submodule_path" ]; then
        echo -e "${YELLOW}Warning: Submodule directory ${local_submodule_path} does not exist${NC}"
        return 1
    fi

    # Get local commit
    local local_submodule_commit=$(cd "$local_submodule_path" && git rev-parse HEAD 2>/dev/null)
    if [ -z "$local_submodule_commit" ]; then
        echo -e "${RED}Error: Could not get commit hash from ${local_submodule_path} (not a git repository?)${NC}"
        return 1
    fi

    # Get local upstream URL
    local local_upstream_url=$(cd "$local_submodule_path" && git config --get remote.origin.url 2>/dev/null)

    # Extract flake information
    local flake_commit=$(extract_flake_commit "$submodule_name")
    local flake_repo_url=$(extract_flake_repo_url "$submodule_name")

    # Handle case where flake commit not found
    if [ -z "$flake_commit" ]; then
        echo -e "${YELLOW}⚠ ${local_submodule_path} vs ${flake_file}: Flake Commit Not Found${NC}"
        echo -e " ${YELLOW}Local commit: ${local_submodule_commit:0:7}...${NC}"
        if [ -n "$local_upstream_url" ]; then
            echo -e " ${CYAN}Local upstream: ${local_upstream_url}${NC}"
        fi
        check_upstream_status "$local_submodule_path" "$local_submodule_commit" "$local_upstream_url" "" 
        return 0
    fi

    # Display repository information
    display_repo_info "$local_submodule_path" "$flake_file" "$local_upstream_url" "$flake_repo_url"

    # Compare commits
    if [[ "$local_submodule_commit" == "$flake_commit"* ]]; then
        echo -e "  ${GREEN}✓ Commits MATCH${NC}"
        echo -e "  ${BOLD}Local commit:  ${local_submodule_commit:0:7}...${NC}"
        echo -e "  ${BOLD}Flake commit:  ${flake_commit:0:7}...${NC}"
        check_upstream_status "$local_submodule_path" "$local_submodule_commit" "$local_upstream_url" "$flake_commit" 
    else
        echo -e "  ${RED}✗ Commits MISMATCH${NC}"
        echo -e "  ${RED}Local commit:  ${local_submodule_commit:0:7}...${NC}"
        echo -e "  ${RED}Flake commit:  ${flake_commit:0:7}...${NC}"
        display_commit_comparison "$local_submodule_path" "$local_submodule_commit" "$flake_commit"
        check_upstream_status "$local_submodule_path" "$local_submodule_commit" "$local_upstream_url" "$flake_commit" 
    fi   # Compare commits
}

# Check all git submodules and dump information about them.
# Primary use case: check if the commits being used in Simula's
# flakes are the same as the local submodules
checkCommitConsistencies() {
    echo -e "${BOLD}========================================================================${NC}"
    echo -e "${BOLD}Checking commit consistency for Simula submodules...${NC}"
    echo -e "${BOLD}========================================================================${NC}"
    echo ""

    local submodules=("gdwlroots" "godot" "godot-haskell" "godot-haskell-cabal" "environments" "i3status" "monado" "wlroots")
    local matches=0
    local mismatches=0
    local warnings=0

    for submodule in "${submodules[@]}"; do
        output=$(checkCommitConsistency "$submodule" 2>&1)
        echo "$output"

        # Count results based on the actual output
        if echo "$output" | grep -q "✓ Commits MATCH"; then
            ((matches++))
        elif echo "$output" | grep -q "✗ Commits MISMATCH"; then
            ((mismatches++))
        elif echo "$output" | grep -q "⚠.*Flake Commit Not Found"; then
            ((warnings++))
        elif echo "$output" | grep -q "Warning: Submodule directory"; then
            ((warnings++))
        fi

        echo ""
    done

    echo -e "${BOLD}========================================================================${NC}"
    echo -e "${BOLD}Summary:${NC}"
    echo -e "  ${GREEN}✓ Matches: ${matches}${NC}"
    echo -e "  ${RED}✗ Mismatches: ${mismatches}${NC}"
    echo -e "  ${YELLOW}⚠ Warnings: ${warnings}${NC}"
    echo -e "${BOLD}========================================================================${NC}"
}
