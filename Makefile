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
#	echo '  ypsilon'

prepare:
	cd tests; gcc $(CFLAGS) -shared -Wall -o functions.so functions.c

test: sagittarius mosh vicare racket guile larceny
	@echo done!

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -Lsrc tests/test.scm

vicare: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L src tests/test.scm

# Seems Mosh as well
mosh: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; mosh --loadpath=src tests/test.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; nmosh --loadpath=src tests/test.scm

racket: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ./src tests/test.scm

# guile doesn't read .sls or .guile.sls by default...
prepare-guile:
	echo \(set! %load-extensions \'\(\".guile.sls\" \".sls\" \".scm\"\)\) > .guile.rc

guile: prepare prepare-guile
	LD_LIBRARY_PATH=$(LDPATH):tests; guile --no-auto-compile -l .guile.rc -L src tests/test.scm
	rm .guile.rc

prepare-larceny:
	cd tests; gcc -m32 -shared -Wall -o functions.so functions.c

# Larceny raises an error if PFFI.log is there...
larceny: prepare-larceny
	rm -f PFFI.log
	LD_LIBRARY_PATH=$(LDPATH):tests; larceny -path src -r6rs -program tests/test.scm
