
LDPATH=$(LD_LIBRARY_PATH)

prepare:
	cd tests; gcc -fPIC -shared -o callback.so callback.c

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -L ../src callback.scm

vicare: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L ../src callback.scm

# Seems Mosh as well
mosh: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; mosh --loadpath=../src callback.scm

racket: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ../src callback.scm
