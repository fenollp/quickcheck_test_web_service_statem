all:
	rebar3 compile

debug:
	rebar3 do compile,shell

clean:
	rebar3 clean
