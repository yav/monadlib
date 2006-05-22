.PHONY: ghc hugs install doc dist clean


# These make it easy to install the library on my machine.
# Something similar should work on other machines too.
ghc:
	@echo "********** Building for GHC **********"
	@runhaskell Setup.hs configure --ghc -p
	@runhaskell Setup.hs build
	@runhaskell Setup.hs haddock

hugs:
	@echo "********** Building for Hugs **********"
	@runhaskell Setup.hs configure --with-cpphs=cpphs-hugs --hugs
	@runhaskell Setup.hs build
	@runhaskell Setup.hs haddock

install:
	@runhaskell Setup.hs install

doc:
	@echo "********** Building documentation **********"
	@runhaskell Setup.hs haddock
	@echo "********* Done: documentation should be in 'dist/doc'"


dist:
	@echo "********** Building distribution **********"
	@runhaskell Setup.hs sdist
	@echo "********** Done: distribution should be in 'dist'"
clean:
	@-rm -rf dist Monad/*.hi Monad/*.o .setup-config


