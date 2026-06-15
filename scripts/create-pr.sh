#!/usr/bin/env bash
set -euo pipefail

# Create a GitHub PR.
# Usage: scripts/create-pr.sh --title "PR title" --body "PR body text"

TITLE=""
BODY=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --title) TITLE="$2"; shift 2 ;;
    --body)  BODY="$2";  shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

if [[ -z "$TITLE" ]]; then
  echo "Error: --title is required" >&2
  exit 1
fi

echo "Creating pull request..."
gh pr create --title "$TITLE" --body "$BODY"
