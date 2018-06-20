# Environment variables:
#   + MAPLEAPP: path to the Maple executable.  This cannot be named
#     MAPLE as this would break on Cygwin.
#   + NO_NETWORK: if set, make will skip trying fetching submodules

ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif

MLA=encyclopedia.mla
OK=encyclopedia.ok

all: init ind.m maintable.m rmmla $(OK)

init:
	make -C gdev compilation

ind.m maintable.m: database.mpl enc_functions.mpl createtable.mpl
	$(MAPLEAPP) -b gdev/gdev.mla -B < createtable.mpl

rmmla:
	rm -f $(MLA)

$(MLA):
	echo "march('create', \"$@\", 20) ;" | $(MAPLEAPP) -s -q

algolib_compile: init $(OK)

# Do not do any "rm" for this target, as it is used by algolib!
$(OK): $(MLA) ind.m maintable.m saveall.mpl encyclopedia.mpl
	$(MAPLEAPP) -b $(MLA) -B saveall.mpl && touch $(OK)
