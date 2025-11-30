DUNE = opam exec -- dune

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "	\033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	$(DUNE) build @all

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build --profile=prod @all

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build -w @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build @runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build @runtest --auto-promote

.PHONY: format
format: ## Format the codebase with ocamlformat
	$(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	$(DUNE) build @fmt

.PHONY: init
setup-githooks: ## Setup githooks
	git config core.hooksPath .githooks

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.4.0 --deps-only --no-install -y

.PHONY: install
install: ## Install dependencies
	opam install . --deps-only --with-test --with-dev-setup --with-doc -y

.PHONY: init
init: setup-githooks create-switch install ## Create a local dev enviroment

.PHONY: bench
bench: ## Run benchmark (make bench [quick|compare|scaling|edge|all])
	@$(DUNE) exec benchmark/main.exe --profile=benchmark -- $(filter-out bench,$(MAKECMDGOALS))

# Allow "make bench <scenario>" syntax
quick compare scaling edge all:
	@true

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst

.PHONY: documentation
documentation: ## Generate odoc documentation
	$(DUNE) build --root . @doc-new

.PHONY: documentation-watch
documentation-watch: ## Generate odoc documentation
	$(DUNE) build --root . -w @doc-new

.PHONY: documentation-serve
documentation-serve: documentation ## Open odoc documentation with default web browser
	open _build/default/_doc_new/html/docs/local/quickjs/index.html
