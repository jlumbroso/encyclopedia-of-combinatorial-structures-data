ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif

SRC=$(shell cat package_list.txt | while read n ; do echo $$n.mpl ; done)
TESTS=$(shell cat test_list.txt | while read n ; do echo $$n.test ; done)
HELP_PAGES = $(shell find help -name '*.mws')
MLA=gdev.mla
HDB=gdev.hdb

.SUFFIXES:
.SUFFIXES: .mpl .date .test .ok .mws

all: compilation testing

clean:
	rm -f `find . -name '*.date' -o -name '*.out' -o -name '*.tim' -o -name '*.ok' -o -name '*.mla' -o -name '*.hdb'` failed_tests.txt

compilation: $(MLA)
	$(MAKE) algolib_compile

algolib_compile: $(SRC:.mpl=.date)

testing: $(MLA)
	$(MAKE) -j 4 tests

tests: $(TESTS:.test=.ok)

algolib_test: testing
	for n in $(TESTS:.test=.ok) ; do [ -e $$n ] || echo gdev/$$n ; done >> ../failed_tests.txt

$(MLA):
	rm -f $@
	echo "march('create', \"$@\", 150) ;" | $(MAPLEAPP) -s -q

.mpl.date:
	sed -e 's/#savelib/savelib/g' $< | $(MAPLEAPP) -s -q -e2 -b $(MLA) -B && touch $*.date

.test.ok: $(MLA)
	$(MAPLEAPP) -s -t -b $(MLA) -B $< >$*.out 2>$*.tim
	grep -v -e \# -e okay $*.out || touch $*.ok

algolib_help: $(HELP_PAGES:.mws=.ok)

.mws.ok:
	@n=`echo $< | cut -d/ -f2- | sed -e 's-.mws--g'` ; \
	l=$(<:.mws=.loc) ; \
	[ -e $$l ] || { echo Non-existent $$l ; exit 1 ; } ; \
	{ cat helptool.mpl ; \
	  echo 'helptool("'$<'","'$$n'",'`cat $$l`', "'$(HDB)'") :' ; } \
	| $(MAPLEAPP) | grep rror && { echo Problem with $$l ; exit 1 ; } || \
	{ true ; touch $*.ok ; }
