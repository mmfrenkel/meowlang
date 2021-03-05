#!/bin/bash

printf "POINTLESS TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/pointless.meow
printf "\n\nCOUNT TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/count.meow
printf "\n\nVARIABLES TEST --------------------------------->\n"
./src/meowlang.native < test_programs/variables.meow
printf "\n\nHELLO WORLD TEST ------------------------------->\n"
./src/meowlang.native < test_programs/hello_world.meow
printf "\n\nARRAY TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/arrays.meow
printf "\n\nCONDITIONALS TEST ------------------------------>\n"
./src/meowlang.native < test_programs/conditionals.meow
printf "\n\nLOOPS TEST ------------------------------------->\n"
./src/meowlang.native < test_programs/loops.meow
printf "\n\nCLASS TEST #1 ---------------------------------->\n"
./src/meowlang.native < test_programs/mouse_class.meow
printf "\n\nCLASS TEST #2 ---------------------------------->\n"
./src/meowlang.native < test_programs/mouse_class_two.meow