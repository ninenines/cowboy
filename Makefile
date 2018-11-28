# See LICENSE for licensing information.

PROJECT = cowboy
PROJECT_DESCRIPTION = Small, fast, modern HTTP server.
PROJECT_VERSION = 2.6.1
PROJECT_REGISTERED = cowboy_clock

# Options.

PLT_APPS = public_key ssl
CT_OPTS += -ct_hooks cowboy_ct_hook [] # -boot start_sasl

# Dependencies.

LOCAL_DEPS = crypto

DEPS = cowlib ranch
dep_cowlib = git https://github.com/ninenines/cowlib 2.7.0
dep_ranch = git https://github.com/ninenines/ranch 1.7.1

DOC_DEPS = asciideck

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper gun proper
dep_ct_helper = git https://github.com/extend/ct_helper master
dep_gun = git https://github.com/ninenines/gun master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-19+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
AUTO_CI_WINDOWS ?= OTP-19+

# Standard targets.

include erlang.mk

# Don't run the examples test suite by default.

ifndef FULL
CT_SUITES := $(filter-out examples ws_autobahn,$(CT_SUITES))
endif

# Compile options.

ERLC_OPTS += +warn_missing_spec +warn_untyped_record
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

# Generate rebar.config on build.

app:: rebar.config

# Dialyze the tests.

DIALYZER_OPTS += --src -r test

# h2spec setup.

GOPATH := $(ERLANG_MK_TMP)/gopath
export GOPATH

H2SPEC := $(GOPATH)/src/github.com/summerwind/h2spec/h2spec
export H2SPEC

# @todo It would be better to allow these dependencies to be specified
# on a per-target basis instead of for all targets.
test-build:: $(H2SPEC)

$(H2SPEC):
	$(gen_verbose) mkdir -p $(GOPATH)/src/github.com/summerwind
	$(verbose) git clone --depth 1 https://github.com/summerwind/h2spec $(dir $(H2SPEC))
	$(verbose) $(MAKE) -C $(dir $(H2SPEC)) build MAKEFLAGS=

# Use erl_make_certs from the tested release during CI
# and ensure that ct_helper is always recompiled.

ci-setup:: clean deps test-deps
	$(gen_verbose) cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/ || true
	$(gen_verbose) $(MAKE) -C $(DEPS_DIR)/ct_helper clean app
