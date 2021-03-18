# Use this file to perform regression tests on the meowlang compiler.
# This file is designed to be used from the project root directory and run as:
# ./test/run_regression_tests.sh [run_type]

# helper function to print usage if incorrect args are passed
Usage() {
        echo "Usage ./test/run_regression_tests.sh [test-type] [files]"
        echo "[test-type]  : mandatory; supported test-types are -a (AST: Scanner/Parser Only)"
        echo "[files]      : optional; list of .meow files, default uses all files"
        exit 1
}

# checks to make sure that a source file that should pass still passes
# and compares old result with the new result. must call only after
# the test_type_dir and the run_type global variables are set by the script
Check() {
        test_file=$1      # name of the test file to run
        should_pass=$2    # do we expect the test to pass?

        base_name=$(basename $file .meow)                                        # e.g., "test_conditionals"
        actual_output="$base_name.out"                                           # e.g., "test_conditionals.out"
        expected_output="./test/test_output/$test_type_dir/$base_name.out"       # e.g., ./test/test_output/ast/test_conditions.out

        echo -e "\n*** Running $base_name *** " | tee -a $global_log

        # run the test
        ./src/meowlang.native $run_type < $test_file &> $actual_output

        # see if the result is what we expected
        if [[ $should_pass && $? -ne 0 ]];
        then
                echo "TEST FAILED: Test unexpectedly failed" | tee -a $global_log
                exit 1
        else [[ !$should_pass && $? -eq 0 ]];
        fi

        # run this twice to be sure to easily capture output in global log
        diff -b $actual_output $expected_output >> $global_log
        diff -b $actual_output $expected_output
        if [ $? -ne 0 ];
        then
                echo "TEST FAILED: Result did not match expected output" | tee -a $global_log
                exit 1
        fi

        # report result and remove the old files
        echo "TEST PASSED" | tee -a $global_log
        rm -f $actual_output
}

# ------------------- ENTRY POINT TO REGRESSION TESTS ----------------------- #

n_tests_completed=0

if [[ $# -le 0 ]]
then
        echo "Test-type must be specified as a command line argument"
        Usage
fi

# find the test type (e.g., AST, Semantic Checks, Or Whole Shabang)
if [ $1 == "-a" ]
then
        run_type=$1
        test_type_dir="ast"
        global_log="./test/global_log.out"

        # remove old global log, if it still exists
        rm -f $global_log
else
        echo "Command line arg $1 is not yet a supported test type"
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
	                Check $file $true ;;
                *fail*)
	                Check $file $false ;;
	        *)
	                echo "Unknown file type $file, skipping..." ;;
        esac
        let n_tests_completed++
done

echo -e "\n*** $n_tests_completed successful tests completed! Good to go! ***" 2>&1 | tee -a $global_log
