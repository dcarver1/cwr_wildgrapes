#!/bin/bash

# Target directory
TARGET_DIR="data/Vitis"

# Set to false since you are ready to delete
DRY_RUN=false

echo "Scanning for model runs from 2024 and earlier in $TARGET_DIR..."

# Check if we find anything first
if [ -z "$(find "$TARGET_DIR" -type d \( -name "run2024*" -o -name "run2023*" -o -name "run2022*" \))" ]; then
    echo "No older run folders found matching the patterns."
    exit 0
fi

if [ "$DRY_RUN" = true ]; then
    echo "=========================================================="
    echo "DRY RUN MODE ENABLED: No files will be deleted."
    echo "=========================================================="
    # Using a while loop to safely handle spaces in folder names
    find "$TARGET_DIR" -type d \( -name "run2024*" -o -name "run2023*" -o -name "run2022*" \) | while IFS= read -r folder; do
        echo "Would delete: \"$folder\""
    done
    echo "=========================================================="
else
    echo "=========================================================="
    echo "DELETE MODE ENABLED: Permanently removing folders..."
    echo "=========================================================="
    # Using a while loop to safely handle spaces in folder names
    find "$TARGET_DIR" -type d \( -name "run2024*" -o -name "run2023*" -o -name "run2022*" \) | while IFS= read -r folder; do
        echo "Deleting \"$folder\"..."
        rm -rf "$folder"
    done
    echo "Cleanup complete."
fi