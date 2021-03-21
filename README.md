# meowlang

## Compile:

From root or src directories:
```
$ make
```

## Test

To run an individual test:
```
$ ./src/meowlang.native < test/test_programs/<test_program_name>.meow
```
Currently the following options are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks
* `-c`: Compile to LLVM (limited)
If not specified, `-a` is used.

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

## Hello World

The meowlang compiler currently only supports the hello world program. This can
be run via the following, after compiling the compiler:
```
$ test/test_single_program.sh test_hello_world.meow
```
