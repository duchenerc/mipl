#!/bin/bash
# Packs all Python source files and Makefiles into a zip file.
#
filename="duchener.zip"

rm -f "$filename"
find . -type f -iname "*.py" -not -path "*.vscode*" -print | zip "$filename" -@
find . -type f -iname "Makefile" -print | zip "$filename" -@
