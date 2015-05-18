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
#	echo '  ypsilon'

prepare:
	cd tests; gcc $(CFLAGS) -shared -o functions.so functions.c

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -Lsrc tests/test.scm

vicare: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L src tests/test.scm

# Seems Mosh as well
mosh: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; mosh --loadpath=src tests/test.scm

racket: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ./src tests/test.scm
