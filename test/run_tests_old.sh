#!/bin/bash

if [[ $1 ]]
then
    arg=$1
else
    arg="-a"
fi

printf "POINTLESS TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_pointless.meow ${arg} > test/test_output/test_pointless_ast.out
printf "\n\nCOUNT TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_count.meow ${arg} > test/test_output/test_count_ast.out
printf "\n\nVARIABLES TEST --------------------------------->\n"
./src/meowlang.native < test/test_programs/test_variables.meow ${arg} > test/test_output/test_variables_ast.out
printf "\n\nHELLO WORLD TEST ------------------------------->\n"
./src/meowlang.native < test/test_programs/test_hello_world.meow ${arg} > test/test_output/test_hello_world_ast.out
printf "\n\nARRAY TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_create_array.meow ${arg} > test/test_output/test_create_array_ast.out
printf "\n\nCONDITIONALS TEST ------------------------------>\n"
./src/meowlang.native < test/test_programs/test_conditionals.meow ${arg} > test/test_output/test_conditionals_ast.out
printf "\n\nLOOPS TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_loops.meow ${arg} > test/test_output/test_loops_ast.out
printf "\n\nCLASS TEST #1 ---------------------------------->\n"
./src/meowlang.native < test/test_programs/test_mouse_class.meow ${arg} > test/test_output/test_mouse_class_ast.out
printf "\n\nCLASS TEST #2 ---------------------------------->\n"
./src/meowlang.native < test/test_programs/test_mouse_class_two.meow ${arg} > test/test_output/test_mouse_class_two_ast.out
