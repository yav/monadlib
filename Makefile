# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2003/12/15 18:10:10 diatchki Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = 

ALL_DIRS = \
	Unstable/Control/Monad \
	Unstable/Control/Monad/Private \
	Unstable/Control/Monad/ST

PACKAGE=monads
PACKAGE_DEPS=haskell98

SRC_HC_OPTS += -Wall -fglasgow-exts 

SRC_HADDOCK_OPTS += -t "Monad Libraries (${PACKAGE} package)"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
