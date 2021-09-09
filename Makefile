export IDRIS2 ?= idris2

lib_pkg = webidl.ipkg

test_pkg = test.ipkg

.PHONY: all
all: lib test

.PHONY: lib
lib:
	${IDRIS2} --build ${lib_pkg}

.PHONY: test
test:
	${IDRIS2} --build ${test_pkg} && build/exec/runTest -n 1000

.PHONY: clean
clean:
	${IDRIS2} --clean ${lib_pkg}
	${IDRIS2} --clean ${test_pkg}
	${RM} -r build

.PHONY: develop
develop:
	find -name "*.idr" | entr -d idris2 --typecheck ${lib_pkg}
