#!/usr/bin/env bash
# Install the estimation environment.
set -euo pipefail
ddm_package_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ddm_runtime_dir="${1:-$ddm_package_dir/runtime}"
mkdir -p "$ddm_runtime_dir"
ddm_runtime_dir="$(cd "$ddm_runtime_dir" && pwd)"
if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
    printf '%s\n' 'This explicit environment lock is for Linux x86-64.' >&2
    exit 1
fi
env CONDA_REGISTER_ENVS=false \
    CONDA_PKGS_DIRS="$ddm_runtime_dir/pkgs" \
    CONDA_ENVS_PATH="$ddm_runtime_dir/envs" \
    CONDA_NOTIFY_OUTDATED_CONDA=false \
    conda create --yes --prefix "$ddm_runtime_dir/hddm_py27" \
      --file "$ddm_package_dir/code/environment.txt"
printf 'Estimator Python: %s\n' "$ddm_runtime_dir/hddm_py27/bin/python"
