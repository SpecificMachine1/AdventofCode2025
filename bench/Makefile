############################################################
#			 Variables			   #
############################################################

# environment
top_srcdir = $(abspath ../)
SHELL := bash
VPATH := .:..

# interpreters and flags
CHEZ = chez
CHEZFLAGS = --libdirs .:.. --program
GUILE = guile
GUILEFLAGS = -L . -L .. -x .guile.sls -x .sls --no-debug 

# dependencies
days := $(wildcard day/*)
data := $(wildcard data/*)
bench_deps := $(days) $(data)

############################################################
#			   Rules			   #
############################################################
#.PHONY: check clean help

help:  ## print this message
	grep -E '^[^ :]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: guile-bench chez-bench ## run all benchs


guile-bench: profile.scm $(bench_deps) ## test against Guile
	$(GUILE) $(GUILEFLAGS) $< | tee benchmarks/guile.csv 

chez-bench: profile.scm $(bench_deps) ## test against Chez
	$(CHEZ) $(CHEZFLAGS) $< | tee benchmarks/chez.csv 


clean: ## delete test files in ./test-logs
	rm -rf benchmarks/*.csv
