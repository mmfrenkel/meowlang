#!/bin/bash
LLC="llc"                             # LLVM compiler
CC="cc"                               # C compiler

# get the path of the file, regardless of directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
MEOWLANG="$DIR/../src/meowlang.native"

# Paths to the relevant .o file to link into compiler
CUSTOM_SCAN="$DIR/../src/custom_scanf.o"
CUSTOM_STRCMP="$DIR/../src/custom_strcmp.o"
CUSTOM_STRCAT="$DIR/../src/custom_strcat.o"
CUSTOM_CASTING="$DIR/../src/custom_casting.o"

if [[ $# -le 0 ]]
then
        echo -e "Usage: ./meowlang.sh program_name.meow [flags]"
        echo -e "-A (compile and run)"
        echo -e "-c (compile and print)"
        echo -e "-s (semantic check)"
        echo -e "-a (ast pretty-print)"
        exit 1
fi

cd $(dirname "${1}")
name=$(basename $1 .meow)

if [[ $2 == "-a" ]]
then
        $MEOWLANG -f $(basename $1) -a
elif [[ $2 == "-s" ]]
then
        $MEOWLANG -f $(basename $1) -s
elif [[ $2 == "-c" ]]
then
        $MEOWLANG -f $(basename $1) -c
else
        # build and run everything
        $MEOWLANG -f $(basename $1) -c > "$name.ll"

        if [[ $? -eq 0 ]];
        then
                $LLC -relocation-model=pic "$name.ll" > "$name.s" &&
                $CC -o "$name.exe" "$name.s" $CUSTOM_SCAN $CUSTOM_STRCMP $CUSTOM_STRCAT $CUSTOM_CASTING  &&
                "./$name.exe"
        fi
fi
