# See LICENSE for licensing information.

PROJECT = cowboy
PROJECT_DESCRIPTION = Small, fast, modern HTTP server.
PROJECT_VERSION = 2.7.0
PROJECT_REGISTERED = cowboy_clock

# Options.

PLT_APPS = public_key ssl
CT_OPTS += -ct_hooks cowboy_ct_hook [] # -boot start_sasl

# Dependencies.

LOCAL_DEPS = crypto

DEPS = cowlib ranch
dep_cowlib = git https://github.com/ninenines/cowlib 2.8.0
dep_ranch = git https://github.com/ninenines/ranch 1.7.1

DOC_DEPS = asciideck

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper gun proper
dep_ct_helper = git https://github.com/extend/ct_helper master
dep_gun = git https://github.com/ninenines/gun master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-20+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
AUTO_CI_WINDOWS ?= OTP-LATEST-20+

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
	$(verbose) git clone --depth 1 https://github.com/summerwind/h2spec $(dir $(H2SPEC)) || true
	$(verbose) $(MAKE) -C $(dir $(H2SPEC)) build MAKEFLAGS= || true

# Use erl_make_certs from the tested release during CI
# and ensure that ct_helper is always recompiled.

ci-setup:: clean deps test-deps
	$(gen_verbose) cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/ || true
	$(gen_verbose) $(MAKE) -C $(DEPS_DIR)/ct_helper clean app

# Prepare for the release.

prepare_tag:
	$(verbose) echo -n "Most recent tag:            "
	$(verbose) git tag | tail -n1
	$(verbose) git verify-tag `git tag | tail -n1`
	$(verbose) echo -n "MAKEFILE: "
	$(verbose) grep -m1 PROJECT_VERSION Makefile
	$(verbose) echo -n "APP:                 "
	$(verbose) grep -m1 vsn ebin/$(PROJECT).app | sed 's/	//g'
	$(verbose) echo -n "GUIDE:  "
	$(verbose) grep -h dep_$(PROJECT)_commit doc/src/guide/*.asciidoc || true
	$(verbose) echo
	$(verbose) echo "Titles in most recent CHANGELOG:"
	$(verbose) for f in `ls -r doc/src/guide/migrating_from_*.asciidoc | head -n1`; do \
		echo $$f:; \
		grep == $$f; \
	done
	$(verbose) echo
	$(verbose) echo "Dependencies:"
	$(verbose) grep ^DEPS Makefile || echo "DEPS ="
	$(verbose) grep ^dep_ Makefile || true
	$(verbose) echo
	$(verbose) echo "rebar.config:"
	$(verbose) cat rebar.config || true
