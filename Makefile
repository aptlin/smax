emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

INIT = init.el

test: clean
	${CASK_EXEC} ert-runner init
	${CASK_EXEC} ert-runner

build:
	${CASK_EXEC} ${emacs} -batch -l ${INIT}

compile:
	${CASK_EXEC} ${emacs} -Q -batch -l ${INIT} -L "." -f batch-byte-compile *.el

clean:
	rm -f *.elc

travis:
	open https://travis-ci.org/sdll/smax

github:
	open https://github.com/sdll/smax

.PHONY:	all test package clean-elc test-melpa
