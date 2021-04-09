
.PHONY : build_complier build_and_test compress

build_complier :
	cd ./src && $(MAKE) clean && $(MAKE) && cd ../

build_and_test: build_complier
	./test/test_all.sh

compress : build_complier
	cd ./src && $(MAKE) clean && cd ../.. && tar czf meowlang.tar.gz ./meowlang

