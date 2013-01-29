# See LICENSE for licensing information.

PROJECT = cowboy
RANCH_VSN = 0.6.0
ERLC_OPTS = -Werror +debug_info +warn_export_all # +bin_opt_info +warn_missing_spec

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

.PHONY: all clean-all app clean docs clean-docs tests autobahn build-plt dialyze

# Application.

all: app

clean-all: clean clean-docs
	rm -f .$(PROJECT).plt
	rm -rf $(DEPS_DIR) logs

deps/ranch:
	@mkdir -p $(DEPS_DIR)
	git clone -n -- https://github.com/extend/ranch.git $(DEPS_DIR)/ranch
	cd $(DEPS_DIR)/ranch ; git checkout -q $(RANCH_VSN)

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app: deps/ranch ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	@$(MAKE) -C $(DEPS_DIR)/ranch

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ \
		src/$(PROJECT)_middleware.erl $?

clean:
	-@$(MAKE) -C $(DEPS_DIR)/ranch clean
	rm -rf ebin/
	rm -f test/*.beam
	rm -f erl_crash.dump

# Documentation.

docs: clean-docs
	erl -noshell -eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

# Tests.

CT_RUN = ct_run \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs \
	-cover test/cover.spec

tests: ERLC_OPTS += -DTEST=1
tests: clean app
	@mkdir -p logs/
	@$(CT_RUN) -suite eunit_SUITE http_SUITE ws_SUITE

autobahn: clean app
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE

# Dialyzer.

build-plt: app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl $(DEPS_DIR)/ranch

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
