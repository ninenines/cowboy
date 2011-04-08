# See LICENSE for licensing information.

REBAR = rebar

all: app

app:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: app
	@$(REBAR) eunit
	@$(REBAR) ct

dialyze:
	@$(REBAR) dialyze
