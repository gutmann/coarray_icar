TOPTARGETS := all clean uninstall install tests

SUBDIRS := $(wildcard */Makefile)
SUBDIRS := $(subst Makefile,,$(SUBDIRS))
# SUBDIRS := tests

all: $(SUBDIRS)
$(TOPTARGETS): $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TOPTARGETS) $(SUBDIRS)
