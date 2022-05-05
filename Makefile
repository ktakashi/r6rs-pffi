LDPATH=$(LD_LIBRARY_PATH)
LONG_BIT=$(shell getconf LONG_BIT)
CFLAGS_32=
CFLAGS_64=-fPIC
CFLAGS=$(CFLAGS_$(LONG_BIT))

all:
	echo 'usage: make $traget'
	echo '  sagittarius'
	echo '  vicare'
	echo '  mosh'
	echo '  racket'
	echo '  guile'
	echo '  chez'

prepare:
	cd tests; gcc $(CFLAGS) -shared -Wall -o functions.so functions.c

test: sagittarius mosh vicare racket guile larceny
	@echo done!

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	cd tests; sagittarius -L../src test.scm

vicare: prepare
	cd test; vicare -L ../src test.scm

# Seems Mosh as well
mosh: prepare
	cd tests; mosh --loadpath=../src test.scm
	cd tests; nmosh --loadpath=../src test.scm

racket: prepare
	cd tests; plt-r6rs ++path ../src test.scm

# guile doesn't read .sls or .guile.sls by default...
prepare-guile:
	echo \(set! %load-extensions \'\(\".guile.sls\" \".sls\" \".scm\"\)\) > .guile.rc

guile: prepare prepare-guile
	cd tests; guile --no-auto-compile -l .guile.rc -L ../src test.scm
	rm .guile.rc

prepare-larceny:
	cd tests; gcc -m32 -shared -Wall -o functions.so functions.c

# Larceny raises an error if PFFI.log is there...
larceny: prepare-larceny
	rm -f PFFI.log
	cd tests; larceny -path ../src -r6rs -program test.scm

prepare-chez: prepare
	$(shell test ! -f tests/lib/srfi/:64.sls && ln -s %3a64.chezscheme.sls tests/lib/srfi/:64.sls)
	$(shell test ! -d tests/lib/srfi/:64 && ln -s %3a64 tests/lib/srfi/:64)

chez: prepare-chez
	cd tests; scheme --libdirs ../src:lib --program test.scm
	cd tests; scheme --libdirs ../src:lib --program test.chez.scm
