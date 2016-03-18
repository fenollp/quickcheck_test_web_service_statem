all:
	rebar3 compile

debug: all
	rebar3 shell

clean:
	rebar3 clean
