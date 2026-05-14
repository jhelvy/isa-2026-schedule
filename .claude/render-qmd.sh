#!/bin/bash
f=$(jq -r '.tool_input.file_path // empty' 2>/dev/null)
[[ "$f" == *.qmd ]] || exit 0
cd "/Users/jhelvy/gh/web/isa-2026-schedule" && quarto render "$(basename "$f")"
