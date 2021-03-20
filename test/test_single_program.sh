
LLC="llc"                               # LLVM compiler
CC="cc"                                 # C compiler
MEOWLANG="./src/meowlang.native"        # Meowlang compiler
PROGRAM_PATH="test/test_programs"

if [[ $# -le 0 ]]
then
        echo -e "Test file must be provided as a command line arg \n"
        echo "./test/test_single_program <program_name.meow>"
fi

# build everything
name=$(basename $1 .meow)

$MEOWLANG < "$PROGRAM_PATH/$1" -c > "$name.ll"
$LLC -relocation-model=pic "$name.ll" > "$name.s" &&
$CC -o "$name.exe" "$name.s" &&
"./$name.exe"

# clean up
rm -f "$name.ll" "$name.s" "$name.ll"