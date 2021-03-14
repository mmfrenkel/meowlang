# meowlang

From root directory:

Compile:
```
$ make
```

To run an individual test:
```
$ ./src/meowlang.native < test_programs/<test_program_name>.meow
```
Currently the following options are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks
If not specified, `-a` is used.

You can run regression tests for the project with the following command:
```
$ ./test/run_regression_tests.sh [run-type] [files]
```
Specifying the `run-type` is mandatory. Options for `run-type` currently include:
* `-a`: Test output of scanner and parser (against pretty printed AST)

Specifying a list of `files` is optional. By default, all files with a `.meow` extension
 will be run as a test.
