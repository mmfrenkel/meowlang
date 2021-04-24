[![Build Status](https://travis-ci.com/mmfrenkel/meowlang.svg?token=A82yQjjqXq4xHFY2Wdxo&branch=main)](https://travis-ci.com/mmfrenkel/meowlang)

# meowlang

Meowlang is an object-oriented esoteric programming language inspired by LOLCODE (Adam Lindsay, 2007).

The syntax is inspired by internet lolspeak, characterized by intentionally misspelled
and grammatically incorrect natural language.

Nevertheless, Meowlang is a general purpose programming language, providing
support for user-defined classes and functions, arrays, and imports.

## I. Compile Compiler

To compile the compiler, from `/` or `src/` directories:
```
$ make
```

## II. Compile & Run .meow Programs

### 0. Hello World

To run our hello world program from the root directory:
```
$ ./bin/meowlang.sh ./test/test_programs/test_hello_world.meow
```

### i. Individual Program

To run the complier against any `.meow` file, use the `meowlang.sh` script found
in `/bin` from any directory and provide the relative path of your file:
```
$ ./bin/meowlang.sh </path/to/meow/file.meow>
```

## III. Testing Indivudal Components of Compiler
```
$ ./bin/meowlang.sh <program_name.meow> [flags]
```
Currently the following options are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks
* `-c`: Compile to LLVM and print target output.

If flag is not specified, then the program will be fully compiled and also run.

### ii. Regression Test Suite

You can run all regression tests for the project with the following command:
```
$ ./test/test_all.sh
```

To run a regression test group (i.e., ast/semantic/full pipeline) individually:
```
$ ./test/run_regression_tests.sh [run-type]
```
Specifying the `run-type` is mandatory. Options for `run-type` currently include:
* `-a`: Test output of scanner and parser (against pretty printed AST)
* `-s`: Test the semantic checker only
* `-c`: Test the full pipeline (compile the `.meow` test file and run it!)
