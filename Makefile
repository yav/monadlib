# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2003/06/05 00:49:36 diatchki Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = 

ALL_DIRS = \
	Unstable/Control/Monad \
	Unstable/Control/Monad/Private \

PACKAGE=monads
PACKAGE_DEPS=

SRC_HC_OPTS += -Wall -fglasgow-exts 

SRC_HADDOCK_OPTS += -t "Monad Libraries (${PACKAGE} package)"

PACKAGE_CPP_OPTS += -DPACKAGE=\"${PACKAGE}\"
PACKAGE_CPP_OPTS += -DPACKAGE_DEPS='$(patsubst %,"%"$(comma),$(PACKAGE_DEPS)) "haskell98"'
PACKAGE_CPP_OPTS += -DLIBRARY=\"HS$(PACKAGE)\"


# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
