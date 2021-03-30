.PHONY : build

build :
	cd ./src && $(MAKE) clean && $(MAKE)


compress :
	cd ./src && $(MAKE) clean && cd ../.. && tar czf meowlang.tar.gz ./meowlang