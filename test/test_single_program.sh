
LLC="llc"                               # LLVM compiler
CC="cc"                                 # C compiler
MEOWLANG="./src/meowlang.native"        # Meowlang compiler
PROGRAM_PATH="test/test_programs"

if [[ $# -le 0 ]]
then
        echo -e "Test file must be provided as a command line arg \n"
        echo "./test/test_single_program <program_name.meow>"
        echo "Add --keep flag to keep all files after run is complete"
fi

# build everything
name=$(basename $1 .meow)

$MEOWLANG < "$PROGRAM_PATH/$1" -c > "$name.ll"

if [[ $? -eq 0 ]];
then
        $LLC -relocation-model=pic "$name.ll" > "$name.s" &&
        $CC -o "$name.exe" "$name.s" "./src/custom_scanf.o" "./src/custom_casting.o" &&
        "./$name.exe"
fi

# clean up, unless --keep flag is provided
if [[ $# -lt 2 || $2 != "--keep" ]]
then
        rm -f "$name.ll" "$name.s" "$name.exe"
fi
