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
	opam install . --deps-only --with-test -y

.PHONY: init
init: setup-githooks create-switch install ## Create a local dev enviroment

.PHONY: demo
demo: ## Run demo executable
	@$(DUNE) exec demo/demo.exe \
		--profile=demo \
		--action-stdout-on-success=print \
		--display-separate-messages \
		--force \
		--terminal-persistence=clear-on-rebuild \
		--watch

.PHONY: demo-watch
demo-watch: ## Run demo executable
	@$(DUNE) exec demo/demo.exe \
		--watch \
		--action-stdout-on-success=print \
		--display-separate-messages \
		--force \
		--terminal-persistence=clear-on-rebuild \
		--profile=demo

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

.PHONY: release
release: ## Create and push a release tag (usage: make release VERSION=1.0.0)
	@if [ -z "$(VERSION)" ]; then \
		echo "Error: VERSION is required"; \
		echo "Usage: make release VERSION=1.0.0"; \
		exit 1; \
	fi
	@.github/scripts/create-release.sh $(VERSION)
