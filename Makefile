LDPATH=$(LD_LIBRARY_PATH)

all:
	echo 'usage: make $traget'
	echo '  sagittarius'
	echo '  vicare'
	echo '  mosh'
#	echo '  ypsilon'

prepare:
	cd tests; gcc -fPIC -shared -o functions.so functions.c

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -Lsrc tests/test.scm

vicare: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L src tests/test.scm

# Seems Mosh as well
mosh: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; mosh --loadpath=src tests/test.scm
