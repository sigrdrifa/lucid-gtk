.PHONY: help
help: ## print make targets 
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## builds the project with cabal
	cabal build lucid-gtk-ui

.PHONY: run
run:  ## runs the project with cabal
	cabal run lucid-gtk-ui

.PHONY: test
test: ## runs the unit test project
	cabal test lucid-gtk-test --enable-tests --test-show-details=direct

.PHONY: generate-resources
generate-resources: ## generates the resources file
	sh compile-resources.sh

.PHONY: clean
clean: ## cleans the project
	cabal clean
	rm -rf dist
	rm -rf csrc/resources.c

.PHONY: haddock
haddock: ## generates the haddock documentation
	cabal haddock lucid-gtk-ui

.PHONY: repl
repl: ## starts the repl
	cabal repl lucid-gtk-ui

.PHONY: install
install: ## installs the project
	cabal install --overwrite-policy=always
