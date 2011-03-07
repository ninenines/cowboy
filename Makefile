# See LICENSE for licensing information.

all: app

app:
	@./rebar compile

clean:
	@./rebar clean
	rm -f erl_crash.dump

test:
	@./rebar eunit
