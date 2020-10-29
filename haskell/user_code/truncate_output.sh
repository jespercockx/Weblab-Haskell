#!/usr/bin/env bash
set -o nounset
set -o pipefail
set -o errexit
shopt -s nullglob
#set -o xtrace

sizelimit=450000        # bytes

## Version 2.0
## Uses GNU stat

# Truncate tags with long texts in results.xml
if [ -f output/results.xml ] && [ "$(stat --printf="%s" output/results.xml)" -ge $sizelimit ]; then
    perl -i -pe 'BEGIN{undef $/;} s|<\s*(\w+)(\s+[^>]+)?\s*>([^<]{10000})[^<]*<\/\s*(\w+)\s*>|<\1\2>\3\[... truncated\]</\4>|smg' "output/results.xml"
fi
# Remove all text if the file is still too big
if [ -f output/results.xml ] && [ "$(stat --printf="%s" output/results.xml)" -ge $sizelimit ]; then
    perl -i -pe 'BEGIN{undef $/;} s|<\s*(\w+)(\s+[^>]+)?\s*>[^<]*<\/\s*(\w+)\s*>|<\1\2></\3>|smg' "output/results.xml"
fi

# Truncate other output files
for f in output/*; do
    if [ -e "$f" ] && [ "$f" != "output/results.xml" ] && [ "$(stat --printf="%s" $f)" -ge $sizelimit ]; then
        truncate -s $(echo $sizelimit - 50 | bc) $f && \
        echo "[... truncated]" >> "$f"
    fi
done
