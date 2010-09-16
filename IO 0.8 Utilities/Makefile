#
#	Makefile for the Clean iolib utilities
#

#
#		make nothing (default):
#			don't do anything
#
#		make install:
#			install files
#
#		make cleanup:
#			remove all files that can be recreated
#

nothing : 

CLEAN_MODULES = \
	Help Highscore Random

IOUTILS_FILES = \
	$(CLEAN_MODULES:%=%.dcl) \
	$(CLEAN_MODULES:%=%.icl)

#
# Install section
#

# INSTALL_IOUTILS_DIR = $(CURRENTDIR)
INSTALL_IOUTILS_FILES = $(IOUTILS_FILES:%=$(INSTALL_IOUTILS_DIR)/%)

install: $(INSTALL_IOUTILS_DIR) $(INSTALL_IOUTILS_FILES)

$(INSTALL_IOUTILS_DIR) :
	mkdir -p $@

$(INSTALL_IOUTILS_DIR)/% : %
	cp $< $@
	chmod 644 $@

CURRENTDIR = $(MAKECURRENTDIR)$(GMAKECURRENTDIR)
MAKECURRENTDIR : sh=pwd
GMAKECURRENTDIR = ${shell echo `pwd`}


#
# Cleanup
#

cleanup:
	$(RM) Clean\ System\ Files/*.abc
	$(RM) Clean\ System\ Files/*.o

