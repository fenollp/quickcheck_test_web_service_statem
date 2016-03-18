all:
	rebar3 compile

debug:
	rebar3 do compile,shell

clean:
	$(if $(wildcard test/.eqc-info test/*.eqc), rm test/.eqc-info test/*.eqc)
	$(if $(wildcard test/stateful_service_eqc.beam), rm test/stateful_service_eqc.beam)
	rebar3 clean

distclean: clean
	$(if $(wildcard _build), rm -r _build)
