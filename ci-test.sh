#!/bin/bash

declare -a implementations=($(scheme-env list -l))

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

cd tests
for impl in ${implementations[@]}; do
    echo Testing with ${impl}
    name=${impl%@*}
    for file in *.scm; do
	case $file in
	    $name.test.scm)
		scheme-env run ${impl} \
			   --loadpath ../src \
			   --loadpath lib \
			   --standard r6rs --program ${file} | check_output
		;;
	    test.scm)
		scheme-env run ${impl} \
			   --loadpath ../src \
			   --loadpath lib \
			   --standard r6rs --program ${file} | check_output
		;;
	    *)
		# Do nothing
		;;
	esac
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done
cd ..

echo Library test status ${EXIT_STATUS}
exit ${EXIT_STATUS}
