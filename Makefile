.PHONY: all debug test eqc proper clean

all:
	rebar3 compile

debug:
	rebar3 shell

test: proper
eqc:
	rebar3 eqc -p prop_ticket_dispenser
proper:
	rebar3 as test proper

clean:
	rebar3 clean

distclean: clean
	$(if $(wildcard .eqc-info), rm .eqc-info)
	$(if $(wildcard *.eqc), rm *.eqc)
	$(if $(wildcard .eqc/), rm -r .eqc/)
	$(if $(wildcard .rebar3/), rm -r .rebar3/)
	$(if $(wildcard test/.rebar3/), rm -r test/.rebar3/)
	$(if $(wildcard _build), rm -r _build)
