.DEFAULT_GOAL := help

R := Rscript --no-init-file -e

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

.PHONY: help tests clean

document: ## refresh function documentation
	$(R) "devtools::document()"

build: document ## build package
	$(R) "devtools::build(vignettes = FALSE)"

check: ## check package
	$(R) "devtools::check(cran = TRUE, vignettes = TRUE)" 2>&1 | tee out-check.txt

styler: ## styler package
	$(R) "styler::style_dir('.')"

tests: ## run test
	$(R) "Sys.setenv('TESTTHAT_MAX_FAILS' = Inf); devtools::test()" 2>&1 | tee out-testthat.txt

build_site: ## build pkgdown site
	$(R) "pkgdown::build_site()"

install_deps: ## install dependencies
	$(R) 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'if (!requireNamespace("dotenv")) install.packages("dotenv")' \
	-e 'if (file.exists(".env")) dotenv::load_dot_env()' \
	-e 'remotes::install_deps(dependencies = TRUE, upgrade = "never")'

install_remote: ## install package from github
	$(R) 'remotes::install_github("brunobrr/bdc", force_github = TRUE)'

install: install_deps build ## install package
	cd ..; \
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

quick_install: build ## quick install (document, build, install) tar package version (used in development)
	cd ..; \
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean: ## clean *.tar.gz *.Rcheck
	cd ..; \
	$(RM) -rv $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck

README.md: README.Rmd ## render README
	$(R) "rmarkdown::render('$<')"

render: ## force render README
	$(R) "rmarkdown::render('README.Rmd')"

eg:     ## run examples
	$(R) "devtools::run_examples(run_dontrun = TRUE)"

gp:     ## get goodpractice' suggestions
	$(R) "goodpractice::gp()" &> out-gp.txt

all: install tests check ## run install_deps, build, install, tests, and check

check_all: check tests gp ## run check, tests, and gp

help:         ## show this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
