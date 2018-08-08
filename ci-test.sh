#!/bin/bash

declare -a implementations=(sagittarius@0.9.2 chez@v9.5)

echo "Preparing for Chez Scheme"
create_symlink() {
    flag=$1
    target=$2
    src=$3
    if [ ! ${flag} ${src} ]; then
	ln -s ${target} ${src}
    fi
}
create_symlink -f %3a64.chezscheme.sls tests/lib/srfi/:64.sls
create_symlink -d %3a64 tests/lib/srfi/:64

check_output() {
    local status=0
    while IFS= read -r LINE; do
	echo $LINE
	case $LINE in
	    *FAIL*) status=255 ;;
	    *Exception*) status=255 ;;
	esac
    done
    return ${status}
}

EXIT_STATUS=0

echo "Preparing for tests"

gcc -fPIC -shared -Wall -o tests/functions.so tests/functions.c

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:tests

for impl in ${implementations[@]}; do
    echo Testing with ${impl}
    for file in tests/*.scm; do
	scheme-env run ${impl} \
		   --loadpath src \
		   --loadpath tests/lib \
		   --standard r6rs --program ${file} | check_output
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done

echo Library test status ${EXIT_STATUS}
exit ${EXIT_STATUS}