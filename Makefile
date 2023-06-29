KERNELS = $(wildcard codegen/Elm/Kernel/*.elm)
ELM_CORE_VERSION = 1.0.5

.PHONY: all
all: generated/Core/Basics.elm

generated/Core/Basics.elm: codegen/Gen/Basics.elm codegen/Generate.elm node_modules/elm-codegen/bin/elm-codegen build/src/core-${ELM_CORE_VERSION}/src/Basics.elm build/src/codegen/Elm/Kernel/List.elm
	yarn elm-codegen run --flags-from build/src

codegen/Gen/Basics.elm: codegen/elm.codegen.json node_modules/elm-codegen/bin/elm-codegen
	yarn elm-codegen install

node_modules/elm-codegen/bin/elm-codegen: package.json yarn.lock
	yarn install
	touch -c $@

build/elm-core-${ELM_CORE_VERSION}.tar.gz:
	mkdir -p build
	curl -sSL https://github.com/elm/core/archive/refs/tags/${ELM_CORE_VERSION}.tar.gz -o $@

build/src/core-${ELM_CORE_VERSION}/src/Basics.elm: build/elm-core-${ELM_CORE_VERSION}.tar.gz
	mkdir -p build/src
	tar -xf $< -C build/src -m
	sed -i.bck 's/n-1/n - 1/g' build/src/core-${ELM_CORE_VERSION}/src/List.elm
	rm build/src/core-${ELM_CORE_VERSION}/src/List.elm.bck

build/src/codegen/Elm/Kernel/List.elm: ${KERNELS}
	mkdir -p build/src/codegen
	cp -r codegen/Elm build/src/codegen
