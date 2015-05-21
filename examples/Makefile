LONG_BIT=$(shell getconf LONG_BIT)
CFLAGS_32=
CFLAGS_64=-fPIC
CFLAGS=$(CFLAGS_$(LONG_BIT))

LDPATH=$(LD_LIBRARY_PATH)

prepare:
	gcc $(CFLAGS) -shared -o callback.so callback.c
	gcc $(CFLAGS) -shared -o struct.so struct.c
	gcc $(CFLAGS) -shared -o variable.so variable.c

# Sagittarius and Vicare read shared object from LD_LIBRARY_PATH
sagittarius: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -L ../src struct.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -L ../src variable.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; sagittarius -L ../src callback.scm

vicare: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L ../src struct.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L ../src variable.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; vicare -L ../src callback.scm

# Seems Mosh as well
mosh: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; nmosh --loadpath=../src struct.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; nmosh --loadpath=../src variable.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; nmosh --loadpath=../src callback.scm

racket: prepare
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ../src struct.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ../src variable.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; plt-r6rs ++path ../src callback.scm


# guile doesn't read .sls or .guile.sls by default...
prepare-guile:
	echo \(set! %load-extensions \'\(\".guile.sls\" \".sls\" \".scm\"\)\) > .guile.rc

guile: prepare prepare-guile
	LD_LIBRARY_PATH=$(LDPATH):tests; guile --no-auto-compile -l .guile.rc -L ../src struct.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; guile --no-auto-compile -l .guile.rc -L ../src variable.scm
	LD_LIBRARY_PATH=$(LDPATH):tests; guile --no-auto-compile -l .guile.rc -L ../src callback.scm
	rm .guile.rc

