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
GUILEFLAGS = -L . -L .. -x .sls --no-auto-compile --no-debug

# dependencies
days := $(wildcard day/*)
data := $(wildcard data/*)
testing_deps := $(days) $(data)

############################################################
#			   Rules			   #
############################################################
#.PHONY: check clean help

help:  ## print this message
	grep -E '^[^ :]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

guile-test: r6rs-test.scm $(testing_deps) ## test against Guile
	$(GUILE) $(GUILEFLAGS) $< | tee test-logs/guile.csv 

chez-test: r6rs-test.scm $(testing_deps) ## test against Chez
	$(CHEZ) $(CHEZFLAGS) $< | tee test-logs/chez.csv 

