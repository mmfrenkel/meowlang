# Use the script when you're prepared to save the output from a test run
# of a test program. This will run the compiler against just the program,
# with each of the available flags.

if [[ $# -le 0 ]]
then
        echo "Test file name must be specified as a command line argument"
else
        test_file=$1
fi

basename=$(basename $1 .meow)

./src/meowlang.native < test/test_programs/$test_file -a &> ./test/test_output/ast/$basename.out
./src/meowlang.native < test/test_programs/$test_file -s &> ./test/test_output/semantic/$basename.out

case $test_file in
        *test*)
                ./test/test_single_program.sh $test_file > ./test/test_output/full_pipeline/$basename.out
                ;;
        *fail*)
                ./src/meowlang.native < test/test_programs/$test_file -c &> ./test/test_output/full_pipeline/$basename.out
                ;;
        *)
                echo "Unknown file type $file, skipping..." ;;
esac
