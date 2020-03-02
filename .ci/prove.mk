OUTDIR=$(shell pwd)/out

PROVE_MODE ?= all
WARNINGS_AS_ERRORS ?= --warnings=error

prove_spark_ci:
	make -C .ci/lsc NO_SPARK=1 DESTDIR=$(OUTDIR)/lsc install
	gnatprove --mode=$(PROVE_MODE) -j0 --prover=z3,cvc4 --steps=1000 \
		--memlimit=1000 --checks-as-errors $(WARNINGS_AS_ERRORS) -U -P .ci/cbe.gpr
