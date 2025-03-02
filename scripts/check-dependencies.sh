#!/usr/bin/env bash
set -euo pipefail

check_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "❌ $1 is not installed"
        return 1
    else
        echo "✅ $1 is installed"
        return 0
    }
}

check_stack_version() {
    local required_version="2.9.1"  # adjust as needed
    local current_version
    current_version=$(stack --version | awk '{print $2}')
    if [ "$(printf '%s\n' "$required_version" "$current_version" | sort -V | head -n1)" = "$required_version" ]; then
        echo "✅ stack version $current_version is compatible"
        return 0
    else
        echo "❌ stack version $current_version is not compatible (requires >= $required_version)"
        return 1
    fi
}

# Check for required tools
MISSING_DEPS=0

echo "Checking system dependencies..."

# Essential build tools
check_command "git" || ((MISSING_DEPS++))
check_command "stack" || ((MISSING_DEPS++))
check_command "task" || ((MISSING_DEPS++))
check_command "watchexec" || ((MISSING_DEPS++))
check_command "sass" || ((MISSING_DEPS++))

# Check Stack version
check_stack_version || ((MISSING_DEPS++))

if [ $MISSING_DEPS -gt 0 ]; then
    echo "❌ Found $MISSING_DEPS missing dependencies"
    echo "Please run 'task install-deps' or install them manually"
    exit 1
else
    echo "✅ All dependencies are satisfied"
fi

