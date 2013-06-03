# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http spdy ws
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = ranch
TEST_DEPS = ct_helper
dep_ranch = https://github.com/extend/ranch.git 0.8.3
dep_ct_helper = https://github.com/extend/ct_helper.git master

# Standard targets.

include erlang.mk

# Extra targets.

.PHONY: autobahn

autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE
