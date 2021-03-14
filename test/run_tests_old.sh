#!/bin/bash

if [[ $1 ]]
then
    arg=$1
else
    arg="-a"
fi

printf "POINTLESS TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_pointless.meow ${arg} > test/test_output/ast/test_pointless.out
printf "\n\nCOUNT TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_count.meow ${arg} > test/test_output/ast/test_count.out
printf "\n\nVARIABLES TEST --------------------------------->\n"
./src/meowlang.native < test/test_programs/test_variables.meow ${arg} > test/test_output/ast/test_variables.out
printf "\n\nHELLO WORLD TEST ------------------------------->\n"
./src/meowlang.native < test/test_programs/test_hello_world.meow ${arg} > test/test_output/ast/test_hello_world.out
printf "\n\nARRAY TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_create_array.meow ${arg} > test/test_output/ast/test_create_array.out
printf "\n\nCONDITIONALS TEST ------------------------------>\n"
./src/meowlang.native < test/test_programs/test_conditionals.meow ${arg} > test/test_output/ast/test_conditionals.out
printf "\n\nLOOPS TEST ------------------------------------->\n"
./src/meowlang.native < test/test_programs/test_loops.meow ${arg} > test/test_output/ast/test_loops.out
printf "\n\nCLASS TEST #1 ---------------------------------->\n"
./src/meowlang.native < test/test_programs/test_mouse_class.meow ${arg} > test/test_output/ast/test_mouse_class.out
printf "\n\nCLASS TEST #2 ---------------------------------->\n"
./src/meowlang.native < test/test_programs/test_mouse_class_two.meow ${arg} > test/test_output/ast/test_mouse_class_two.out


# Failing Tests
./src/meowlang.native < test/test_programs/fail_syntax_variables.meow -a 2> test/test_output/ast/fail_syntax_variables.out
./src/meowlang.native < test/test_programs/fail_syntax_comment.meow -a 2> test/test_output/ast/fail_syntax_comment.out
./src/meowlang.native < test/test_programs/fail_syntax_function1.meow -a 2> test/test_output/ast/fail_syntax_function1.out
./src/meowlang.native < test/test_programs/fail_syntax_function2.meow -a 2> test/test_output/ast/fail_syntax_function2.out
./src/meowlang.native < test/test_programs/fail_syntax_function3.meow -a 2> test/test_output/ast/fail_syntax_function3.out