KERNELS = $(wildcard codegen/Elm/Kernel/*.elm)
ELM_CORE_VERSION = 1.0.5

.PHONY: all
all: generated/Core/Basics.elm

generated/Core/Basics.elm: build/modules.elms codegen/Gen/Basics.elm codegen/Generate.elm node_modules/elm-codegen/bin/elm-codegen
	yarn elm-codegen run --flags-from $<

codegen/Gen/Basics.elm: codegen/elm.codegen.json node_modules/elm-codegen/bin/elm-codegen
	yarn elm-codegen install

node_modules/elm-codegen/bin/elm-codegen: package.json yarn.lock
	yarn install
	touch $@

build/elm-core-${ELM_CORE_VERSION}.tar.gz:
	mkdir -p build
	curl -sSL https://github.com/elm/core/archive/refs/tags/${ELM_CORE_VERSION}.tar.gz -o $@

build/core-${ELM_CORE_VERSION}/src/Basics.elm: build/elm-core-${ELM_CORE_VERSION}.tar.gz
	(cd build; tar -xf elm-core-${ELM_CORE_VERSION}.tar.gz)
	touch $@

build/modules.elms: ${KERNELS} build/core-${ELM_CORE_VERSION}/src/Basics.elm Makefile
	mkdir -p build
	(find build/core-${ELM_CORE_VERSION}/src -type f -name '*.elm'; find codegen/Elm  -type f -name '*.elm') | xargs awk 'FNR==1 && NR!=1 {print "---SNIP---"}{print}' | sed 's/n-1/n - 1/' > $@
