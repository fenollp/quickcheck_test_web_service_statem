EBIN = _build/default/lib/mylib/ebin

all:
	rebar3 compile

debug:
	rebar3 do compile,shell

test: QC ?= proper
test: test-$(QC)
compile-test-$(QC): all
	erlc -o $(EBIN) test/$(QC)_stateful_service.erl
test-$(QC): compile-test-$(QC)
	erl -pa $(EBIN)/ \
	  -eval '{ok,_} = application:ensure_all_started(mylib)' \
	  -eval '$(QC):quickcheck($(QC)_stateful_service:prop_ticket_dispenser())' \
	  -s init stop

clean:
	$(if $(wildcard test/.eqc-info test/*.eqc), rm test/.eqc-info test/*.eqc)
	rebar3 clean

distclean: clean
	$(if $(wildcard _build), rm -r _build)
