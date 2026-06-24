#!/usr/bin/env bash
set -euo pipefail

INPUT_DIR="plots_backup3"
OUTPUT_DIR="plots_small"

# Max dimensions. Images smaller than this are not enlarged.
MAX_SIZE="1600x1600>"

mkdir -p "$OUTPUT_DIR"

find "$INPUT_DIR" -type f -iname "*.png" | while IFS= read -r file; do
    rel_path="${file#$INPUT_DIR/}"
    out_file="$OUTPUT_DIR/$rel_path"
    out_dir="$(dirname "$out_file")"

    mkdir -p "$out_dir"

    echo "Optimizing: $file -> $out_file"

    convert "$file" \
        -strip \
        -resize "$MAX_SIZE" \
        -define png:compression-level=9 \
        -define png:compression-filter=5 \
        -define png:compression-strategy=1 \
        "$out_file"
done

echo "Done. Optimized PNGs are in: $OUTPUT_DIR"
