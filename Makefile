.PHONY: all
all: build test package

.PHONY: init
init:
	cabal sandbox init
	cabal clean

.PHONY: build
build: init
	cabal install --only-dependencies --enable-tests --enable-benchmarks \
	--with-hsc2hs=`which hsc2hs`
	cabal configure -flib-Werror --enable-tests --enable-benchmarks -v2 -O2
	cabal build

.PHONY: test
test:
	cabal test --show-details=always --test-options="-a 1000 \
	--maximum-unsuitable-generated-tests=100000 --color"
	cabal bench || true

.PHONY: package
package: test
	cabal check
	# tests that a source-distribution can be generated
	cabal sdist
	# check that the generated source-distribution can be built & installed
	{ \
	set -e; \
	export SRC_TGZ=$$(cabal info . | awk '{print $$2 ".tar.gz";exit}') ; \
    cd dist/; \
	cabal sandbox init; \
    if [ -f "$$SRC_TGZ" ]; then \
       cabal install --force-reinstalls "$$SRC_TGZ"; \
    else \
       echo "expected '$$SRC_TGZ' not found"; \
       exit 1; \
    fi; \
	}
