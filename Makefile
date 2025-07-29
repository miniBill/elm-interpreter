LIBRARIES = elm/core/1.0.5 elmcraft/core-extra/2.2.0

.PHONY: all
all: generated/Core/Basics.elm

generated/Core/Basics.elm: codegen/Gen/Basics.elm codegen/Generate.elm node_modules/elm-codegen/bin/elm-codegen $(patsubst %,build/src/%/elm.json,$(LIBRARIES)) build/src/codegen/Elm/Kernel/List.elm
	yarn elm-codegen run --flags-from build/src

codegen/Gen/Basics.elm: codegen/elm.codegen.json node_modules/elm-codegen/bin/elm-codegen $(wildcard helpers/*.elm)
	yarn elm-codegen install

node_modules/elm-codegen/bin/elm-codegen: package.json yarn.lock
	yarn install
	touch -c $@

.PRECIOUS: build/%.tar.gz
build/%.tar.gz:
	set -e &&\
	NAME=$$(echo $* | cut -d/ -f1,2) &&\
	VERSION=$$(echo $* | cut -d/ -f3) &&\
	mkdir -p $(dir $@) &&\
	curl -sSL https://github.com/$$NAME/archive/refs/tags/$$VERSION.tar.gz -o $@

build/src/%/elm.json: build/%.tar.gz
	mkdir -p $(@D)
	tar -xf $< --strip-components=1 -C $(@D) -m

build/src/codegen/Elm/Kernel/List.elm: $(wildcard codegen/Elm/Kernel/*.elm)
	mkdir -p build/src/codegen
	cp -r codegen/Elm build/src/codegen

ALL_GENERATED = $(shell find generated -type f -name '*.elm')
ALL_SRC = $(shell find src -type f -name '*.elm')
dist/ui.js: src/UI.elm $(ALL_SRC) generated/Core/Basics.elm $(ALL_GENERATED)
	elm make $< --output $@

.PHONY: measure
measure: dist/ui.js
	du -sh $^
	gzip -9 $^
	du -sh $^.gz
	gunzip $^
	npx elmjs-inspect $^ | head -10
