PROJ          = monadLib
VERSION	      = 1.2

INSTALLDIR    = /usr/local/lib/$(PROJ)
PACKAGE       = $(PROJ)
LIB_NAME      = lib$(PROJ).a
SRC_DIST_NAME = $(PROJ)-$(VERSION)-src.tar.gz

all: $(LIB_NAME) docs


$(LIB_NAME): Monad/*.hs
	ghc --make -O -fglasgow-exts -package-name $(PACKAGE) Monad/*.hs
	ar cqs $(LIB_NAME) Monad/*.o

docs: Monad/*.hs 
	-rm -r docs
	mkdir docs
	-ln -s ../Examples docs/Examples
	haddock -h -k $(PACKAGE) -o docs Monad/*.hs Examples/*.hs

dist: docs Examples/*.hs
	-rm $(PROJ)
	make clean
	mkdir $(PROJ)
	cp -r README Makefile package.conf docs Monad Examples $(PROJ)
	tar -czvf $(SRC_DIST_NAME) $(PROJ)
	rm -r $(PROJ)

install: docs $(LIB_NAME)
	mkdir -p $(INSTALLDIR)
	cp $(LIB_NAME) $(INSTALLDIR)
	mkdir -p $(INSTALLDIR)/Monad
	cp -r Monad/*.hi $(INSTALLDIR)/Monad 
	installdir=$(INSTALLDIR) package=$(PACKAGE) ghc-pkg -u -g < package.conf

clean:	
	-rm Monad/*.o Monad/*.hi

veryclean: clean
	-rm $(LIB_NAME)
	-rm $(SRC_DIST_NAME)
	-rm -r docs

