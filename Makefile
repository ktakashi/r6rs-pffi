LDPATH=$(LD_LIBRARY_PATH)
LONG_BIT=$(shell getconf LONG_BIT)
CFLAGS_32=
CFLAGS_64=-fPIC
CFLAGS=$(CFLAGS_$(LONG_BIT))

CHEZ?=scheme

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
	cd tests; sagittarius -L../src -Llib test.scm

vicare: prepare
	cd test; vicare -L ../src -L lib test.scm

# Seems Mosh as well
mosh: prepare
	cd tests; mosh --loadpath=../src --loadpath=lib test.scm
	cd tests; nmosh --loadpath=../src --loadpath=lib  test.scm

prepare-racket:
# Not sure since when, but Racket requires either platform specific extension
# e.g. dynlib, or no extension.
	cd tests; gcc $(CFLAGS) -shared -Wall -o functions functions.c
# Don't they have oneshot library installation command?
#	raco pkg install -t file -n pffi/helper --pkgs --force pffi-helper.plt
	plt-r6rs --force --install src/pffi/compat.mzscheme.sls
	plt-r6rs --force --install src/pffi/misc.sls
	plt-r6rs --force --install src/pffi/procedure.sls
	plt-r6rs --force --install src/pffi/variable.sls
	plt-r6rs --force --install src/pffi/pointers.sls
	plt-r6rs --force --install src/pffi/struct.sls
	plt-r6rs --force --install src/pffi.sls

racket: prepare prepare-racket
	cd tests; plt-r6rs test.scm

guile: prepare
	cd tests; guile --no-auto-compile --r6rs -L ../src -L lib test.scm

prepare-larceny:
	cd tests; gcc -m32 -shared -Wall -o functions.so functions.c

# Larceny raises an error if PFFI.log is there...
larceny: prepare-larceny
	rm -f PFFI.log
	cd tests; larceny -path ../src -path lib -r6rs -program test.scm

prepare-chez: prepare
	$(shell test ! -f tests/lib/srfi/:64.sls && ln -s %3a64.chezscheme.sls tests/lib/srfi/:64.sls)
	$(shell test ! -d tests/lib/srfi/:64 && ln -s %3a64 tests/lib/srfi/:64)

chez: prepare-chez
	cd tests; $(CHEZ) --libdirs ../src:lib --program test.scm
	cd tests; $(CHEZ) --libdirs ../src:lib --program chez.test.scm
