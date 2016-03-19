EBIN = _build/default/lib/mylib/ebin

all:
	rebar3 compile

debug:
	rebar3 do compile,shell

compile-test: all
#	erlc -o $(EBIN) test/pqc_stateful_service.erl
	erlc -o $(EBIN) test/eqc_stateful_service.erl
test: compile-test
	erl -pa $(EBIN)/ \
	  -eval '{ok,_} = application:ensure_all_started(mylib)' \
	  -eval 'eqc:quickcheck(eqc_stateful_service:prop_ticket_dispenser())' \
	  -s init stop

clean:
	$(if $(wildcard test/.eqc-info test/*.eqc), rm test/.eqc-info test/*.eqc)
	rebar3 clean

distclean: clean
	$(if $(wildcard _build), rm -r _build)
