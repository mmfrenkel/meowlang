# Use the script when you're prepared to save the output from a test run
# of a test program. This will run the compiler against just the program,
# with each of the available flags.
COMPILER=../../src/meowlang.native

if [[ $# -le 0 ]]
then
        echo "Test file name must be specified as a command line argument"
else
        test_file=$1
fi

basename=$(basename $1 .meow)
cd ./test/test_programs

$COMPILER -a -f $test_file &> ../test_output/ast/$basename.out
$COMPILER -s -f $test_file &> ../test_output/semantic/$basename.out

case $test_file in
        *test*)
                cd ../..
                ./test/test_single_program.sh $test_file > ./test/test_output/full_pipeline/$basename.out
                ;;
        *fail*)
                $COMPILER -f $test_file -c &> ../test_output/full_pipeline/$basename.out
                ;;
        *)
                echo "Unknown file type $file, skipping..." ;;
esac
