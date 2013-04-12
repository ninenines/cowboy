# See LICENSE for licensing information.

PROJECT = cowboy
RANCH_VSN = 0.8.1
ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
   +warn_shadow_vars +warn_obsolete_guard # +bin_opt_info +warn_missing_spec

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

# Makefile tweaks.

V ?= 0

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(?F);
erlc_verbose = $(erlc_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

.PHONY: all clean-all app clean deps clean-deps docs clean-docs tests autobahn build-plt dialyze

# Application.

all: deps app

clean-all: clean clean-deps clean-docs
	$(gen_verbose) rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app: ebin/$(PROJECT).app
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

COMPILE_FIRST = src/cowboy_middleware.erl src/cowboy_sub_protocol.erl

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ \
		$(COMPILE_FIRST) $?

clean:
	$(gen_verbose) rm -rf ebin/ test/*.beam erl_crash.dump

# Dependencies.

$(DEPS_DIR)/ranch:
	@mkdir -p $(DEPS_DIR)
	git clone -n -- https://github.com/extend/ranch.git $(DEPS_DIR)/ranch
	cd $(DEPS_DIR)/ranch ; git checkout -q $(RANCH_VSN)

deps: $(DEPS_DIR)/ranch
	@$(MAKE) -C $(DEPS_DIR)/ranch

clean-deps:
	-@$(MAKE) -C $(DEPS_DIR)/ranch clean

# Documentation.

docs: clean-docs
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Tests.

CT_RUN = ct_run \
	-noshell \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs
#	-cover test/cover.spec

tests: ERLC_OPTS += -DTEST=1
tests: clean clean-deps deps app
	@mkdir -p logs/
	@$(CT_RUN) -suite eunit_SUITE http_SUITE ws_SUITE

autobahn: clean clean-deps deps app
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE

# Dialyzer.

build-plt: deps app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl $(DEPS_DIR)/ranch

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
