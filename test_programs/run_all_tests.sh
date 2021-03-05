#!/bin/bash

if [[ $1 ]]
then
    arg=$1
else
    arg="-a"
fi

printf "POINTLESS TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/pointless.meow ${arg}
printf "\n\nCOUNT TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/count.meow ${arg}
printf "\n\nVARIABLES TEST --------------------------------->\n"
./src/meowlang.native < test_programs/variables.meow ${arg}
printf "\n\nHELLO WORLD TEST ------------------------------->\n"
./src/meowlang.native < test_programs/hello_world.meow ${arg}
printf "\n\nARRAY TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/arrays.meow ${arg}
printf "\n\nCONDITIONALS TEST ------------------------------>\n"
./src/meowlang.native < test_programs/conditionals.meow ${arg}
printf "\n\nLOOPS TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/loops.meow ${arg}
printf "\n\nCLASS TEST #1 ---------------------------------->\n"
./src/meowlang.native < test_programs/mouse_class.meow ${arg}
printf "\n\nCLASS TEST #2 ---------------------------------->\n"
./src/meowlang.native < test_programs/mouse_class_two.meow ${arg}
