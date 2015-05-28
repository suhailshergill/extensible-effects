GHCS = 7.6.3 7.8.4 7.10.1

.PHONY: all
all: build test package doc tags

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

.PHONY: doc
doc:
	cabal haddock --internal

.PHONY: tags
tags:
	haskdogs -e

.PHONY: devel
devel: build
	{ \
	DIRS="*.hs *.cabal ./src ./test"; \
	EVENTS="-e modify -e move -e delete"; \
	EXCLUDE="\.#"; \
	while inotifywait -qq $$EVENTS -r $$DIRS --exclude $$EXCLUDE; do \
		make test && make doc && make tags; \
	done; \
	}

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

.PHONY: test-all
test-all: build package

.PHONY: ci-test
ci-test:
	# run tests for all ghc versions
	# TODO: figure out a way to run even if in nix-shell
	{ \
	set -e; \
	if [ "$$IN_NIX_SHELL" ]; then \
		echo "Invoked from within nix-shell; exiting"; \
		exit 1; \
	else \
		for e in $(GHCS); do \
			echo "Testing GHC $$e"; \
			nix-shell shell-$$e.nix --command "make test-all"; \
			echo "DONE GHC $$e"; \
		done; \
	fi; \
	}
