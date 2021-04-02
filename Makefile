.PHONY : build_complier

build: build_complier
	./test/test_all.sh

build_complier :
	cd ./src && $(MAKE) clean && $(MAKE) && cd ../

compress :
	cd ./src && $(MAKE) clean && cd ../.. && tar czf meowlang.tar.gz ./meowlang