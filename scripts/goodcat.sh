#!/usr/bin/env sh
# Usage: ./cat_with_headers.sh file1.txt file2.txt *.log

if [ $# -eq 0 ]; then
    echo "Usage: $0 file1 [file2 ...]" >&2
    exit 1
fi

for file in "$@"; do
    if [ -f "$file" ]; then
        echo "=== $file ==="
        cat "$file"
        echo
        echo
    else
        echo "Warning: $file not found or not a file" >&2
    fi
done
