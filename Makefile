# See LICENSE for licensing information.

PROJECT = cowboy

DIALYZER = dialyzer
REBAR = rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc skip_deps=true

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

# Tests.

deps/proper:
	@$(REBAR) -C rebar.tests.config get-deps
	cd deps/proper && $(REBAR) compile

tests: clean deps/proper app eunit ct

inttests: clean deps/proper app eunit intct

eunit:
	@$(REBAR) -C rebar.tests.config eunit skip_deps=true

ct:
	@$(REBAR) -C rebar.tests.config ct skip_deps=true suites=http,proper,ws

intct:
	@$(REBAR) -C rebar.tests.config ct skip_deps=true suites=http,proper,ws,autobahn

# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
