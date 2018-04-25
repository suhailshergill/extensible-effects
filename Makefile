GHCS = 7.8.4 7.10.3 8.0.2 8.2.2

.PHONY: all
all: build test package doc tags

.PHONY: init
init:
	stack init
	stack clean

.PHONY: build
build: init
	stack build --only-dependencies --enable-tests --enable-benchmarks
	stack build

.PHONY: test
test: build
	stack test --test-arguments="-a 1000 \
	--maximum-unsuitable-generated-tests=100000 --color"

.PHONY: bench
bench:
	stack bench --benchmark-arguments="-o benchmark/benchmarks.html"

.PHONY: doc
doc:
	stack haddock --haddock-internal

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
	# check and bundle the package
	stack sdist # outputs to .stack-work/install/<machine-architecture>/<snapshot>/doc
	# TODO: output bundled target to somewhere else than stack decides

	# TODO: check that the generated source-distribution can be built & installed
	#{ \
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
	# run tests for all ghc versions given in different stack/*.yaml
	{ \
	set -e; \
	for stack_yaml in $$(ls stack); do \
		echo "Testing $$stack_yaml"; \
		stack --stack-yaml="stack/$$stack_yaml" build; \
		stack --stack-yaml="stack/$$stack_yaml" test; \
	done; \
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



.PHONY: stack-8.4.1-build
stack-8.4.1-build:
	stack --stack-yaml=stack/stack-8.4.1.yaml build

.PHONY: stack-8.4.1-test
stack-8.4.1-test: stack-8.4.1-build
	stack --stack-yaml=stack/stack-8.4.1.yaml test

.PHONY: stack-8.4.1-bench
stack-8.4.1-bench: stack-8.4.1-build stack-8.4.1-test
	stack --stack-yaml=stack/stack-8.4.1.yaml bench


.PHONY: stack-8.2.2-build
stack-8.2.2-build:
	stack --stack-yaml=stack/stack-8.2.2.yaml build

.PHONY: stack-8.2.2-test
stack-8.2.2-test: stack-8.2.2-build
	stack --stack-yaml=stack/stack-8.2.2.yaml test

.PHONY: stack-8.2.2-bench
stack-8.2.2-bench: stack-8.2.2-build stack-8.2.2-test
	stack --stack-yaml=stack/stack-8.2.2.yaml bench


.PHONY: stack-8.0.2-build
stack-8.0.2-build:
	stack --stack-yaml=stack/stack-8.0.2.yaml build

.PHONY: stack-8.0.2-test
stack-8.0.2-test: stack-8.0.2-build
	stack --stack-yaml=stack/stack-8.0.2.yaml test

.PHONY: stack-8.0.2-bench
stack-8.0.2-bench: stack-8.0.2-build stack-8.0.2-test
	stack --stack-yaml=stack/stack-8.0.2.yaml bench


.PHONY: stack-7.10.3-build
stack-7.10.3-build:
	stack --stack-yaml=stack/stack-7.10.3.yaml build

.PHONY: stack-7.10.3-test
stack-7.10.3-test: stack-7.10.3-build
	stack --stack-yaml=stack/stack-7.10.3.yaml test

.PHONY: stack-7.10.3-bench
stack-7.10.3-bench: stack-7.10.3-build stack-7.10.3-test
	stack --stack-yaml=stack/stack-7.10.3.yaml bench


.PHONY: stack-7.8.4-build
stack-7.8.4-build:
	stack --stack-yaml=stack/stack-7.8.4.yaml build

.PHONY: stack-7.8.4-test
stack-7.8.4-test: stack-7.8.4-build
	stack --stack-yaml=stack/stack-7.8.4.yaml test

.PHONY: stack-7.8.4-bench
stack-7.8.4-bench: stack-7.8.4-build stack-7.8.4-test
	stack --stack-yaml=stack/stack-7.8.4.yaml bench


.PHONY: stack-all-lts-vers-test
stack-all-lts-test: stack-8.2.2-test stack-8.0.2-test stack-7.10.3-test stack-7.8.4-test


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
stack-all-test: stack-all-lts-test stack-nightly-test

.PHONY: stack-clean
stack-clean:
	stack clean
	# only cleans lts of default stack.yaml

