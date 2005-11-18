# These make it easy to install the library on my machine.
# Something similar should work on other machines too.
ghc:
	runhaskell Setup.hs configure --ghc -p
	runhaskell Setup.hs build
	runhaskell Setup.hs haddock

hugs:
	runhaskell Setup.hs configure --with-cpphs=cpphs-hugs --hugs
	runhaskell Setup.hs build
	runhaskell Setup.hs haddock

install:
	runhaskell Setup.hs install

dist::
	runhaskell Setup.hs sdist


