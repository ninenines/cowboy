# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http ws
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = ranch
dep_ranch = https://github.com/extend/ranch.git 0.8.1

# Standard targets.

include erlang.mk

# Extra targets.

.PHONY: autobahn

autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE
