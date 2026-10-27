.PHONY: help debug test testfast watchprod watchdev buildprod builddev
.DEFAULT_GOAL := help

FILTER_NPM = npm --prefix filter_js run
R_TEST = Rscript -e 'devtools::test(stop_on_failure = TRUE)'

help: ## List available targets
	@awk 'BEGIN {FS = ":.*## "} /^[a-zA-Z_-]+:.*## / {printf "  %-10s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

debug: ## Start Rscript under cgdb
	cgdb /usr/bin/Rscript

test: ## Run the full test suite, including shinytest2 tests
	LOCAL_SHINY_TESTS=true $(R_TEST)

testfast: ## Run the test suite, skipping shinytest2 tests
	LOCAL_SHINY_TESTS=true SKIP_SHINY_TESTS=true $(R_TEST)

watchprod: ## Rebuild the filter JS bundle on change (prod config)
	$(FILTER_NPM) watch:prod

watchdev: ## Rebuild the filter JS bundle on change (dev config)
	$(FILTER_NPM) watch

buildprod: ## Build the filter JS bundle (prod config)
	$(FILTER_NPM) build

builddev: ## Build the filter JS bundle (dev config)
	$(FILTER_NPM) build:dev

# Makevars -> PKG_CFLAGS = -I../inst/include/ -Werror -Wstrict-prototypes -Wold-style-definition -DDEBUG -O0 -g
# Makevars -> PKG_CFLAGS = -I../inst/include/ -Werror -Wstrict-prototypes -Wold-style-definition
