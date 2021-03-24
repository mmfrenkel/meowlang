# Use this file to perform regression tests on the meowlang compiler.
# This file is designed to be used from the project root directory and run as:
# ./test/run_regression_tests.sh [run_type]

# helper function to print usage if incorrect args are passed
Usage() {
        echo "Usage ./test/run_regression_tests.sh [-a|-s|-c] [files]"
        echo "Test type must be specified: "
        echo "  -a (AST: Scanner/Parser Only)"
        echo "  -s (Semantic: Run Semantic Checks)"
        echo "  -c (Compile: Compile to LLVM, Execute)"
        echo "[files]: optional; list of .meow files, default uses all files"
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

        echo -e "\n*** Running $base_name ($test_type_dir) *** " | tee -a $global_log

        # run the relevant test
        if [ $run_type == "-a" ] || [ $run_type == "-s" ]
        then
                # semantic and ast checks are simple to test
                ./src/meowlang.native $run_type < $test_file &> $actual_output
        else
                # testing full compilation requires help from test script
                ./test/test_single_program.sh "$base_name.meow" &> $actual_output
        fi

        # see if the result is what we expected
        if [[ $should_pass && $? -ne 0 ]];
        then
                echo "TEST FAILED: Test unexpectedly failed" | tee -a $global_log
                exit 1
        elif [[ !$should_pass && $? -eq 0 ]];
        then
                echo "TEST FAILED: Test unexpectedly passed" | tee -a $global_log
                exit 1
        fi

        # if we don't have the output file yet, just return
        if [[ ! -f "$expected_output" ]]; then
                echo "$expected_output does not yet exist, so cannot calculate diff."
                rm -f $actual_output
                return 0
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
global_log="./test/global_log.out"

if [[ $# -le 0 ]]
then
        echo "Test-type must be specified as a command line argument"
        Usage
else
        run_type=$1
fi

# find the test type (e.g., AST, Semantic Checks, Or Whole Shabang)
if [ $run_type == "-a" ]
then
        echo "*****************************************"
        echo " RUNNING CHECKS ON ABSTRACT SYNTAX TREE" | tee -a $global_log
        echo "*****************************************"
        test_type_dir="ast"
elif [ $run_type == '-s' ]
then
        echo "*****************************************"
        echo "     RUNNING CHECKS ON SEMANTICS" | tee -a $global_log
        echo "*****************************************"
        test_type_dir="semantic"
elif [ $run_type == "-c" ]
then
        echo "*****************************************"
        echo "    RUNNING CHECKS ON FULL PIPELINE" | tee -a $global_log
        echo "*****************************************"
        test_type_dir="full_pipeline"
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

# remove old global log, if it still exists
rm -f $global_log

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

echo -e "\n*** $n_tests_completed successful $test_type_dir tests completed! Good to go! ***" 2>&1 | tee -a $global_log
