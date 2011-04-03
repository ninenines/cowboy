# See LICENSE for licensing information.

REBAR = rebar

all: app

app:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

test:
	@$(REBAR) eunit

dialyze:
	@$(REBAR) dialyze
