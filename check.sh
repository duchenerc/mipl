
# Where to find sample input files
INPUT="./input"

# Where to place the actual output of running the program
OUTPUT="./output"

# Where to find the expected output
EXPECTED="./expected"

# Where to place generated diffs
REPORTS="./reports"

# The name of the executable file
EXEC="python3 main.py"

green=`tput setaf 2`
red=`tput setaf 1`
reset=`tput sgr0`

inputs=`find $INPUT -type f -iname "$1*.txt"`
fails=0
passes=0

for i in $inputs; do

    filename=$(basename $i) # strip path
    testname="${filename%.*}" # strip extension

    # echo "$EXEC"
    # echo "$INPUT/$filename"
    # echo "$OUTPUT/$filename"

    # actually run file
    ${EXEC} "$INPUT/$filename" > "$OUTPUT/$filename.out"

    # run diff
    diff -yibB "$OUTPUT/$filename.out" "$EXPECTED/$filename.out" > "$REPORTS/$filename"

    # count number of lines in diff
    lines=`diff -ibB $OUTPUT/$filename.out $EXPECTED/$filename.out | wc -l`

    if [ "$lines" -gt "0" ]; then
        fails=$[ $fails + 1 ]
        echo "check: ${red}[fail]${reset} $testname ($lines lines)"
        head "$REPORTS/$filename"
    else
        passes=$[ $passes + 1 ]
        # echo "check: ${green}[pass]${reset} $testname"
    fi

done

echo "check: ${green}$passes tests passed${reset}"
echo "check: ${red}$fails tests failed${reset}"
