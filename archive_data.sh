#!/bin/bash

# Define directories
TARGET_DIR="data"
ARCHIVE_DIR="archive/data_backups"

# Create the archive directory if it doesn't exist
mkdir -p "$ARCHIVE_DIR"

# List of specific folders/files to archive based on the uploaded image. 
# You can easily add or remove items from this list.
ITEMS_TO_ARCHIVE=(
    "daucus"
    "Daucus"
    "jWen_data2025"
    "Mimulus"
    "Pinus"
    "Quercus"
    "Yucca"
)

echo "Starting archival process..."

for item in "${ITEMS_TO_ARCHIVE[@]}"; do
    # Check if the file or directory exists
    if [ -e "$TARGET_DIR/$item" ]; then
        echo "Zipping $item..."
        
        # -r zips recursively, -q makes it quiet (less console spam)
        zip -rq "$ARCHIVE_DIR/${item}_archive.zip" "$TARGET_DIR/$item"
        
        echo "Successfully archived $item to $ARCHIVE_DIR/${item}_archive.zip"
        
        # NOTE: If you want the script to automatically delete the original 
        # folders after zipping, you can uncomment the following line:
        # rm -rf "$TARGET_DIR/$item"
    else
        echo "Warning: $TARGET_DIR/$item not found. Skipping."
    fi
done

echo "Archival complete! Check the $ARCHIVE_DIR directory."