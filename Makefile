.PHONY: help
help: ## print make targets 
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## builds the project with cabal
	cabal build lucid.-gtk-ui

.PHONY: run
run:  ## runs the project with cabal
	cabal run lucid-gtk-ui

.PHONY: test
test: ## runs the unit test project
	cabal test lucid-gtk-test --enable-tests --test-show-details=direct

.PHONY: generate-resources
generate-resources: ## generates the resources file
	sh compile-resources.sh
