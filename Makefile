.PHONY : build

build:
	cd ./src && $(MAKE) clean && $(MAKE)
