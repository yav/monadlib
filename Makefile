# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2003/06/05 18:35:12 diatchki Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = 

ALL_DIRS = \
	Unstable/Control/Monad \
	Unstable/Control/Monad/Private 

PACKAGE=monads
PACKAGE_DEPS=haskell98

SRC_HC_OPTS += -Wall -fglasgow-exts 

SRC_HADDOCK_OPTS += -t "Monad Libraries (${PACKAGE} package)"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
