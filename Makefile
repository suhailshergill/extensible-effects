GHCS = 7.8.4 7.10.3 8.0.2 8.2.2

.PHONY: all
all: build test package doc tags

default.nix: extensible-effects.cabal
	cabal2nix ./. > ./default.nix

.PHONY: init
init:
	cabal sandbox init
	cabal clean

.PHONY: build
build: init
	cabal install --only-dependencies --enable-tests --enable-benchmarks
	cabal configure -flib-Werror --enable-tests --enable-benchmarks -v2 -O2
	cabal build

.PHONY: test
test: build
	cabal test --show-details=always --test-options="-a 1000 \
	--maximum-unsuitable-generated-tests=100000 --color"

.PHONY: bench
bench:
	cabal bench --benchmark-options="-o benchmark/benchmarks.html"

.PHONY: doc
doc:
	cabal haddock --internal

.PHONY: tags
tags:
	haskdogs --hasktags-args=-ex

.PHONY: devel
devel: test
	{ \
	DIRS="*.hs *.cabal ./src ./test ./benchmark"; \
	EVENTS="-e modify -e move -e delete"; \
	EXCLUDE="\.#"; \
	while inotifywait -qq $$EVENTS -r $$DIRS --exclude $$EXCLUDE; do \
		make test && make doc; \
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

.PHONY: stack-build
stack-build:
	stack build

.PHONY: stack-test
stack-test: stack-build
	stack test

.PHONY: stack-bench
stack-bench: stack-build stack-test # only benchmark tested program
	stack bench


.PHONY: stack-8.4.1
stack-8.4.1: stack-8.4.1-build stack-8.4.1-test stack-8.4.1-bench

.PHONY: stack-8.4.1-build
stack-8.4.1-build:
	stack --stack-yaml=stack-8.4.1.yaml build

.PHONY: stack-8.4.1-test
stack-8.4.1-test: stack-8.4.1-build
	stack --stack-yaml=stack-8.4.1.yaml test

.PHONY: stack-8.4.1-bench
stack-8.4.1-bench: stack-8.4.1-build stack-8.4.1-test
	stack --stack-yaml=stack-8.4.1.yaml bench


.PHONY: stack-8.2.2
stack-8.2.2: stack-8.2.2-build stack-8.2.2-test stack-8.2.2-bench

.PHONY: stack-8.2.2-build
stack-8.2.2-build:
	stack --stack-yaml=stack-8.2.2.yaml build

.PHONY: stack-8.2.2-test
stack-8.2.2-test: stack-8.2.2-build
	stack --stack-yaml=stack-8.2.2.yaml test

.PHONY: stack-8.2.2-bench
stack-8.2.2-bench: stack-8.2.2-build stack-8.2.2-test
	stack --stack-yaml=stack-8.2.2.yaml bench


.PHONY: stack-8.0.2
stack-8.0.2: stack-8.0.2-build stack-8.0.2-test stack-8.0.2-bench

.PHONY: stack-8.0.2-build
stack-8.0.2-build:
	stack --stack-yaml=stack-8.0.2.yaml build

.PHONY: stack-8.0.2-test
stack-8.0.2-test: stack-8.0.2-build
	stack --stack-yaml=stack-8.0.2.yaml test

.PHONY: stack-8.0.2-bench
stack-8.0.2-bench: stack-8.0.2-build stack-8.0.2-test
	stack --stack-yaml=stack-8.0.2.yaml bench


.PHONY: stack-8.0.1
stack-8.0.1: stack-8.0.1-build stack-8.0.1-test stack-8.0.1-bench

.PHONY: stack-8.0.1-build
stack-8.0.1-build:
	stack --stack-yaml=stack-8.0.1.yaml build

.PHONY: stack-8.0.1-test
stack-8.0.1-test: stack-8.0.1-build
	stack --stack-yaml=stack-8.0.1.yaml test

.PHONY: stack-8.0.1-bench
stack-8.0.1-bench: stack-8.0.1-build stack-8.0.1-test
	stack --stack-yaml=stack-8.0.1.yaml bench


.PHONY: stack-7.10.3
stack-7.10.3: stack-7.10.3-build stack-7.10.3-test stack-7.10.3-bench

.PHONY: stack-7.10.3-build
stack-7.10.3-build:
	stack --stack-yaml=stack-7.10.3.yaml build

.PHONY: stack-7.10.3-test
stack-7.10.3-test: stack-7.10.3-build
	stack --stack-yaml=stack-7.10.3.yaml test

.PHONY: stack-7.10.3-bench
stack-7.10.3-bench: stack-7.10.3-build stack-7.10.3-test
	stack --stack-yaml=stack-7.10.3.yaml bench


.PHONY: stack-7.8.4
stack-7.8.4: stack-7.8.4-build stack-7.8.4-test stack-7.8.4-bench

.PHONY: stack-7.8.4-build
stack-7.8.4-build:
	stack --stack-yaml=stack-7.8.4.yaml build

.PHONY: stack-7.8.4-test
stack-7.8.4-test: stack-7.8.4-build
	stack --stack-yaml=stack-7.8.4.yaml test

.PHONY: stack-7.8.4-bench
stack-7.8.4-bench: stack-7.8.4-build stack-7.8.4-test
	stack --stack-yaml=stack-7.8.4.yaml bench


.PHONY: stack-all-lts-vers-test
stack-all-lts-vers-test: stack-8.2.2-test stack-8.0.2-test stack-7.10.3-test stack-7.8.4-test
#stack-all-lts-vers-test: stack-8.2.2-test stack-8.0.2-test stack-8.0.1-test stack-7.10.3-test # stack-7.8.4-test

.PHONY: stack-all-lts-vers
stack-all-lts-vers: stack-8.2.2 stack-8.0.2 stack-8.0.1 stack-7.10.3 stack-7.8.4


.PHONY: stack-nightly
stack-nightly: stack-nighly-build stack-nightly-test stack-nightly-bench

.PHONY: stack-nightly-build
stack-nightly-build:
	stack --resolver=nightly build

.PHONY: stack-nightly-test
stack-nightly-test: stack-nightly-build
	stack --resolver=nightly test

.PHONY: stack-nightly-bench
stack-nightly-bench: stack-nightly-build stack-nightly-test
	stack --resolver=nightly bench

.PHONY: stack-all-vers
stack-all-vers: stack-all-lts-vers stack-nightly

.PHONY: stack-clean
stack-clean:
	stack clean

