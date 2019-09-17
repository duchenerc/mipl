rm -f duchener.zip
find . -type f -iname "*.py" -not -path "*.vscode*" -print | zip duchener.zip -@
find . -type f -iname "Makefile" -print | zip duchener.zip -@
