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

To run all tests, run the following command. Currently the following options
are supported as a (mutually exclusive) command line `FLAG`:
* `-a`: Print out abstract syntax tree
* `-s`: Run semantic checks

If not specified, `-a` is used.
```
$ test_programs/run_all_tests.sh [FLAG]
```
