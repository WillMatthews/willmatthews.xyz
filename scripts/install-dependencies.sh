#!/usr/bin/env bash
set -euo pipefail

# Function to install packages based on the package manager
install_packages() {
    if command -v apt-get >/dev/null 2>&1; then
        # Debian/Ubuntu
        sudo apt-get update
        sudo apt-get install -y "$@"
    elif command -v dnf >/dev/null 2>&1; then
        # Fedora
        sudo dnf install -y "$@"
    elif command -v pacman >/dev/null 2>&1; then
        # Arch Linux
        sudo pacman -Syu --noconfirm "$@"
    elif command -v brew >/dev/null 2>&1; then
        # macOS (Homebrew)
        brew install "$@"
    else
        echo "Unsupported package manager. Please install dependencies manually."
        exit 1
    fi
}

# Install Task if not present
if ! command -v task >/dev/null 2>&1; then
    echo "Installing Task..."
    if command -v brew >/dev/null 2>&1; then
        brew install go-task/tap/go-task
    else
        sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b ~/.local/bin
    fi
fi

# Install Stack if not present
if ! command -v stack >/dev/null 2>&1; then
    echo "Installing Stack..."
    curl -sSL https://get.haskellstack.org/ | sh
fi

# Install watchexec if not present
if ! command -v watchexec >/dev/null 2>&1; then
    echo "Installing watchexec..."
    if command -v cargo >/dev/null 2>&1; then
        cargo install watchexec-cli
    else
        # Install from package manager
        install_packages watchexec
    fi
fi

# Install sass if not present
if ! command -v sass >/dev/null 2>&1; then
    echo "Installing sass..."
    npm install -g sass
fi

# Initialize the project
echo "Initializing project..."

# Initialize git submodules if any
git submodule update --init --recursive

# Setup Stack project
stack setup
stack build --only-dependencies

# Create necessary directories
mkdir -p _cache _site posts images templates scss

echo "✅ Dependencies installed successfully!"

