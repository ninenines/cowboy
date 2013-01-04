# See LICENSE for licensing information.

PROJECT = cowboy

REBAR = rebar

all: app

# Application.

deps/ranch:
	@$(REBAR) get-deps

app: deps/ranch
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

# Tests.

deps/proper:
	@$(REBAR) -C rebar.tests.config get-deps
	cd deps/proper && $(REBAR) compile

tests: clean deps/proper app eunit ct

eunit:
	@$(REBAR) -C rebar.tests.config eunit skip_deps=true

CT_RUN = ct_run \
	-pa ebin deps/*/ebin \
	-dir test \
	-logdir logs \
	-cover test/cover.spec

ct:
	@mkdir -p logs/
	@$(CT_RUN) -suite http_SUITE ws_SUITE

autobahn:
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE

# Dialyzer.

DIALYZER = dialyzer

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl deps/*

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
