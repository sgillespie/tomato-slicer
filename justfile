# Get system name (eg x86_64-linux)

system := `nix eval --impure --raw --expr builtins.currentSystem`

# Show available recipes
default:
    @just --list --unsorted

## Building and running

# Build the executable
build:
    nix build ".#pomodoro-waybar-module-hs:exe:pomodoro-waybar-module-hs"

# Run the executable (`just run -- --help`)
run *args:
    nix run ".#pomodoro-waybar-module-hs:exe:pomodoro-waybar-module-hs" -- {{ args }}

# Build release artifacts
dist:
    nix build \
      ".#x86_64-linux-static-dist" \
      ".#x86_64-windows-dist"

## Checks

# Run the static analyzers (statix, deadnix, hlint)
lint:
    nix build \
      ".#checks.{{ system }}.statix" \
      ".#checks.{{ system }}.deadnix" \
      ".#checks.{{ system }}.hlint"

# Format the source tree in place
fmt:
    nix fmt

# Check formatting without writing changes
fmt-check:
    nix build ".#checks.{{ system }}.treefmt"

# Run the test suite
test:
    nix build ".#checks.{{ system }}.pomodoro-waybar-module-hs:test:tests"

# Run basic checks
check-light:
    nix build \
      ".#checks.{{ system }}.statix" \
      ".#checks.{{ system }}.deadnix" \
      ".#checks.{{ system }}.hlint" \
      ".#checks.{{ system }}.treefmt" \
      ".#checks.{{ system }}.pomodoro-waybar-module-hs:test:tests"

# Run the full flake check (every check, all systems)
check-full:
    nix flake check
