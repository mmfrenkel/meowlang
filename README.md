# meowlang

## I. Compile:

From root or src directories:
```
$ make
```

## II. Test

### i. Individual Test

To run an individual test:
```
$ ./src/meowlang.native < test/test_programs/<test_program_name>.meow
```
Currently the following options are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks
* `-c`: Compile to LLVM (limited)
If not specified, `-a` is used.

If you want to compile and run a specific `.meow` file, the best option is to use the `test_single_program.sh` script, specifying the filename (not full path) of the test file. Here is an example of running this script for "hello world":
```
$ test/test_single_program.sh test_hello_world.meow
```

### ii. Regression Test Suite 

You can run regression tests for the project with the following command:
```
$ ./test/run_regression_tests.sh [run-type] [files]
```
Specifying the `run-type` is mandatory. Options for `run-type` currently include:
* `-a`: Test output of scanner and parser (against pretty printed AST)
* `-s`: Test the semantic checker only
* `-c`: Test the full pipeline (compile the `.meow` test file and run it!)

Specifying a list of `files` is optional. By default, all files with a `.meow` extension
will be run as a test.

