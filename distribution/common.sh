#!/bin/bash

export project="elm-dev"
export version="0.6.0"

# Shared by local setup and every distribution target.
read -r stackVersion < "$(dirname "${BASH_SOURCE[0]}")/stack-version"
export stackVersion

# Stack caches completed setup/build work. Retry transient Hackage/download
# failures here, before the macOS scripts strip or upload any release artifacts.
install_stack_with_retry() {
  local stack_command="$1"
  local bin_dir="$2"
  local attempt status

  for attempt in 1 2 3; do
    if "$stack_command" install --local-bin-path "$bin_dir"; then
      return 0
    else
      status=$?
    fi

    # Do not restart a build interrupted by the user or CI cancellation.
    if [[ "$attempt" -eq 3 || "$status" -eq 130 || "$status" -eq 143 ]]; then
      return "$status"
    fi

    echo "Stack install failed (attempt $attempt/3, exit $status); retrying in 15 seconds..." >&2
    sleep 15
  done
}
