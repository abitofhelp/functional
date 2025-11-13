#!/bin/bash
find /Users/mike/Ada/github.com/abitofhelp/tzif/test -name "*.adb" -o -name "*.ads" | while read file; do
  # Fix separator lines with dashes (if any)  
  
  # Also ensure equals separator lines are exactly 75 = signs
  sed -i '' 's/^--  =\{70,80\}$/--  ===========================================================================/' "$file"
done

echo "Fixed all separator lines in test files"
