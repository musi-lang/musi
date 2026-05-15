#!/usr/bin/env sh
set -eu

usage() {
  cat <<'EOF'
Usage: scripts/examples-native-deps.sh [--print|--install]

Installs native libraries used by the examples:
  - libffi: Musi native FFI bridge
  - glfw: window creation examples
  - SDL2: input/window examples
  - sqlite3: C/database interop examples
  - Mesa/OpenGL development files on Linux: graphics examples

Modes:
  --print    Show packages for this system.
  --install  Install packages with the detected package manager.
EOF
}

mode=${1:---print}
case "$mode" in
  --print|--install) ;;
  -h|--help) usage; exit 0 ;;
  *) usage >&2; exit 2 ;;
esac

os=$(uname -s)
manager=
packages=
install_cmd=

case "$os" in
  Darwin)
    if command -v brew >/dev/null 2>&1; then
      manager=brew
      packages="libffi glfw sdl2 sqlite"
      install_cmd="brew install $packages"
    else
      echo "Homebrew is required on macOS: https://brew.sh" >&2
      exit 1
    fi
    ;;
  Linux)
    if command -v apt-get >/dev/null 2>&1; then
      manager=apt
      packages="libffi-dev libglfw3-dev libsdl2-dev libsqlite3-dev libgl1-mesa-dev mesa-common-dev"
      install_cmd="sudo apt-get update && sudo apt-get install -y $packages"
    elif command -v dnf >/dev/null 2>&1; then
      manager=dnf
      packages="libffi-devel glfw-devel SDL2-devel sqlite-devel mesa-libGL-devel mesa-libGLU-devel"
      install_cmd="sudo dnf install -y $packages"
    elif command -v pacman >/dev/null 2>&1; then
      manager=pacman
      packages="libffi glfw sdl2 sqlite mesa"
      install_cmd="sudo pacman -S --needed $packages"
    else
      echo "Supported Linux package manager not found. Install libffi, GLFW, SDL2, SQLite, and OpenGL/Mesa development packages." >&2
      exit 1
    fi
    ;;
  *)
    echo "Unsupported system '$os'. Install libffi, GLFW, SDL2, SQLite, and OpenGL development packages with your system package manager." >&2
    exit 1
    ;;
esac

if [ "$mode" = "--print" ]; then
  echo "manager: $manager"
  echo "packages: $packages"
  echo "install: $install_cmd"
  exit 0
fi

sh -c "$install_cmd"
