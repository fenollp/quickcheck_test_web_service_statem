.PHONY: all debug test eqc clean

all:
	rebar3 compile

debug:
	rebar3 shell

test: eqc
eqc:
	rebar3 as test eqc -p prop_ticket_dispenser

clean:
	rebar3 clean

distclean: clean
	$(if $(wildcard .eqc-info), rm .eqc-info)
	$(if $(wildcard *.eqc), rm *.eqc)
	$(if $(wildcard .eqc/), rm -r .eqc/)
	$(if $(wildcard .rebar3/), rm -r .rebar3/)
	$(if $(wildcard _build), rm -r _build)
