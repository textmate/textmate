#!/usr/bin/env bash
# Theme preview: shell functions, expansions, operators, and here-documents.
set -euo pipefail

readonly APP_NAME="TextMate"
readonly DEFAULT_LIMIT=1024
THEMES=("Light" "Dark" "Author's Choice")
ROOT="${HOME}/Library/Application Support/TextMate"

# Print a named value with a default and an optional color.
log_message() {
    local level="${1:-info}"
    local message="${2:-Hello, world!}"
    printf '[%s] %s\n' "$level" "$message"
}

show_theme() {
    local name="$1"
    case "$name" in
        Light) log_message info "Light appearance" ;;
        Dark)  log_message info "Dark appearance" ;;
        *)     log_message warn "Unspecified: ${name}" ;;
    esac
}

main() {
    local index=0
    for theme in "${THEMES[@]}"; do
        ((index += 1))
        if [[ "$theme" =~ ^[A-Z][a-z]+$ && $index -lt 10 ]]; then
            show_theme "$theme"
        else
            log_message warn "Skipping '${theme}'"
        fi
    done

    local report
    report=$(cat <<REPORT
Theme report
Application: ${APP_NAME}
Location: ${ROOT}
Count: ${#THEMES[@]}
REPORT
)
    printf '%s\n' "$report"
    printf '%s\n' "${THEMES[@]}" | sort | while IFS= read -r name; do
        printf '  %s\n' "${name// /_}"
    done
}

# Single quotes preserve literal expansions; ANSI quotes interpret escapes.
literal='Keep $HOME and $(commands) as literal text.'
escaped=$'Tab:\t newline:\n quote:\''
number=$((0xff + 42 * 2))
trap 'log_message info "Preview finished"' EXIT

main "$@"
printf '%s\n' "$literal" "$escaped" "$number"
