#!/bin/bash

# Script to remove comments from OCaml files
# Exceptions (skip these files):
# - dsp.ml
# - fft.ml
# - serialization.ml
# - reader.ml
# - writer.ml

# Function to remove comments from a file
remove_comments() {
    local file="$1"
    local temp_file=$(mktemp)
    
    # Remove all comments
    awk '
    BEGIN { in_comment = 0 }
    {
        line = $0
        result = ""
        i = 1
        while (i <= length(line)) {
            if (in_comment) {
                # Inside comment - skip until *)
                if (substr(line, i, 2) == "*)") {
                    in_comment = 0
                    i += 2
                } else {
                    i++
                }
            } else {
                # Outside comment
                if (substr(line, i, 2) == "(*") {
                    # Start of comment
                    in_comment = 1
                    i += 2
                } else {
                    result = result substr(line, i, 1)
                    i++
                }
            }
        }
        if (!in_comment) {
            print result
        }
    }
    ' "$file" > "$temp_file"
    
    mv "$temp_file" "$file"
}

# Find all .ml files in lib/ and bin/ directories
find lib bin -name "*.ml" -type f | while read -r file; do
    filename=$(basename "$file")
    
    # Skip specified files
    if [[ "$filename" == "dsp.ml" ]] || \
       [[ "$filename" == "fft.ml" ]] || \
       [[ "$filename" == "serialization.ml" ]] || \
       [[ "$filename" == "reader.ml" ]] || \
       [[ "$filename" == "writer.ml" ]]; then
        echo "Skipping $file"
        continue
    fi
    
    echo "Processing $file..."
    remove_comments "$file"
done

echo "Done!"
