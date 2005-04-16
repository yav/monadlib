# These make it easy to install the library on my machine.
# Something similar should work on other machines too.
ghc:
	runhaskell Setup.hs configure --ghc
	runhaskell Setup.hs build
	runhaskell Setup.hs haddock
	sudo runhaskell Setup.hs install

hugs:
	runhaskell Setup.hs configure --with-cpphs=cpphs-hugs --hugs
	runhaskell Setup.hs build
	runhaskell Setup.hs haddock
	sudo runhaskell Setup.hs install
