export IDRIS2 ?= idris2

lib_pkg = webidl.ipkg

test_pkg = test.ipkg

.PHONY: all lib clean

all: lib test

clean-install: clean install

lib:
	${IDRIS2} --build ${lib_pkg}

test:
	${IDRIS2} --build ${test_pkg} && build/exec/runTest -n 1000

clean:
	${IDRIS2} --clean ${lib_pkg}
	${IDRIS2} --clean ${test_pkg}
	${RM} -r build
