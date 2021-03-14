

# helper function to print usage if incorrect args are passed
Usage() {
        echo "Usage ./test/run_regression_tests.sh [test-type] [files]"
        echo "[test-type]  : mandatory; supported test-types are -a (AST: Scanner/Parser Only)"
        echo "[files]      : optional; list of .meow files, default uses all files"
        exit 1
}


# checks to make sure that a source file that should pass still passes
# and compares old result with the new result
Check() {
        test_file=$1      # name of the test file to run
        run_type=$2       # run type (e.g., -a)
        test_type_dir=$3  # directory containing results for differe test types. e.g., ast)
        should_pass=$4    # do we expect the test to pass?

        base_name=$(basename $file .meow)                                        # e.g., "test_conditionals"
        actual_output="$base_name.out"                                           # e.g., "test_conditionals.out"
        expected_output="./test/test_output/$test_type_dir/$base_name.out"       # e.g., ./test/test_output/ast/test_conditions.out

        echo -e "\n*** Running $base_name *** "

        # run the test
        ./src/meowlang.native $run_type < $test_file &> $actual_output

        # see if the result is what we expected
        if [[ $should_pass && $? -ne 0 ]];
        then
                echo "TEST FAILED: Expected test to succeed, but failed before diff"
                exit 1
        else [[ !$should_pass && $? -eq 0 ]];
        fi

        diff -b $actual_output $expected_output
        if [ $? -ne 0 ]; then
                echo "TEST FAILED: Result did not match expected output"
                exit 1
        fi

        # remove the old files
        echo "TEST PASSED"
        rm -f $actual_output
}

# ------------------- ENTRY POINT TO REGRESSION TESTS ----------------------- #

n_tests_completed=0

if [[ $# -le 0 ]]
then
        echo -e "Test-type must be specified as a command line argument \n"
        Usage
fi

# find the test type (e.g., AST, Semantic Checks, Or Whole Shabang)
if [ $1 == "-a" ]
then
        run_type=$1
        test_type_dir="ast"
        global_log="./test/global_log$suffix.out"

        # remove old global log, if it still exists
        rm -f global_log
else
        echo -e "Command line arg $1 is not yet a supported test type \n"
        Usage
fi

# (1) Get test files to run regression tests on
# if someone passes a specific file or list of files, just run those
if [ $# -ge 2 ]
then
        files=$@
else
        files=$(find ./test/test_programs -type f -name "*.meow")
fi

# (2) Run each file ("test-*.meow" means test should succeed)
for file in $files
do
        case $file in
	        *test*)
	                Check $file $run_type $test_type_dir $true 2>> $global_log
	                ;;

                *fail*)
	                Check $file $run_type $suffix $false 2>> $global_log
	                ;;
	        *)
	                echo "Unknown file type $file"
	                Usage
	                ;;
        esac

        let n_tests_completed++
done

echo -e "\n*** $n_tests_completed successful tests completed! Good to go! ***"
