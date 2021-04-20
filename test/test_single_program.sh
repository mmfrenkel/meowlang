
LLC="llc"                             # LLVM compiler
CC="cc"                               # C compiler
REL_DIR="../../src"                   # src directory relative to test dir
MEOWLANG="$REL_DIR/meowlang.native"   # Meowlang compiler
TEST_PROGRAM_PATH="test/test_programs"

# Paths to the relevant .o file to link into compiler
CUSTOM_SCAN="$REL_DIR/custom_scanf.o"
CUSTOM_STRCMP="$REL_DIR/custom_strcmp.o"
CUSTOM_STRCAT="$REL_DIR/custom_strcat.o"
CUSTOM_CASTING="$REL_DIR/custom_casting.o"

if [[ $# -le 0 ]]
then
        echo -e "Test file must be provided as a command line arg \n"
        echo "./test/test_single_program <program_name.meow>"
        echo "Add --keep flag to keep all files after run is complete"
fi

cd $TEST_PROGRAM_PATH

# build everything
name=$(basename $1 .meow)

$MEOWLANG -f "$1" -c > "$name.ll"

if [[ $? -eq 0 ]];
then
        $LLC -relocation-model=pic "$name.ll" > "$name.s" &&
        $CC -o "$name.exe" "$name.s" $CUSTOM_SCAN $CUSTOM_STRCMP $CUSTOM_STRCAT $CUSTOM_CASTING  &&
        "./$name.exe"
fi

# clean up, unless --keep flag is provided
if [[ $# -lt 2 || $2 != "--keep" ]]
then
        rm -f "$name.ll" "$name.s" "$name.exe"
fi

cd ../.. # go back up to where you where before
