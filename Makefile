INSTALLDIR = /usr/local/lib/MTL
SRCDIR = Unstable/Control/Monad
HSFILES	= $(SRCDIR)/*.hs $(SRCDIR)/Private/*.hs $(SRCDIR)/ST/*.hs
PACKAGE = monads

libMTL.a: dirs utils $(HSFILES) 
	ghc --make -fglasgow-exts -package-name $(PACKAGE) -odir dirs/obj -hidir dirs/hi $(HSFILES)
	-rm -rf dirs/lib
	mkdir dirs/lib
	./utils/renameObjs dirs/obj dirs/lib
	ar cqs libMTL.a dirs/lib/*.o
	-rm -rf dirs/lib

dirs ::
	-mkdir -p dirs/obj
	-mkdir -p dirs/hi


utils ::
	(cd utils; make)

install: 
	mkdir -p $(INSTALLDIR)
	cp libMTL.a $(INSTALLDIR)
	cp -r dirs/hi/* $(INSTALLDIR) 
	installdir=$(INSTALLDIR) package=$(PACKAGE) ghc-pkg -u -g < package.conf


clean:	
	-rm -rf dirs

