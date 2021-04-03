[![Build Status](https://travis-ci.com/mmfrenkel/meowlang.svg?branch=main)](https://travis-ci.com/mmfrenkel/meowlang)


# meowlang

## I. Compile:

To compile the compiler, from `/` or `src/` directories:
```
$ make
```

## II. Test

### 0. Hello World

To run our hello world program:
```
$ test/test_single_program.sh test_hello_world.meow
```

### i. Individual Test

To test the compiler against an individual `.meow` file:
```
$ ./src/meowlang.native < test/test_programs/<test_program_name>.meow
```
Currently the following options are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks
* `-c`: Compile to LLVM (limited)

If flag is not specified, `-a` is used.

If you want to compile **and run** a specific `.meow` file, the best option is
to use the `test_single_program.sh` script, specifying the filename (not full path)
of the test file:
```
$ test/test_single_program.sh <name-of-file>.meow
```

### ii. Regression Test Suite

You can run all regression tests for the project with the following command:
```
$ ./test/test_all.sh
```

To run a regression test group (i.e., ast/semantic/full pipeline) individually:
```
$ ./test/run_regression_tests.sh [run-type] [files]
```
Specifying the `run-type` is mandatory. Options for `run-type` currently include:
* `-a`: Test output of scanner and parser (against pretty printed AST)
* `-s`: Test the semantic checker only
* `-c`: Test the full pipeline (compile the `.meow` test file and run it!)

Specifying a list of `files` is optional. By default, all files with a `.meow` extension
will be run as a test.

