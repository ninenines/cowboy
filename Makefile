# See LICENSE for licensing information.

PROJECT = cowboy
PROJECT_DESCRIPTION = Small, fast, modern HTTP server.
PROJECT_VERSION = 2.13.0
PROJECT_REGISTERED = cowboy_clock

# Options.

PLT_APPS = public_key ssl # ct_helper gun common_test inets
CT_OPTS += -ct_hooks cowboy_ct_hook [] # -boot start_sasl
#CT_OPTS += +JPperf true +S 1

# Dependencies.

LOCAL_DEPS = crypto

DEPS = cowlib ranch
dep_cowlib = git https://github.com/ninenines/cowlib 2.14.0
dep_ranch = git https://github.com/ninenines/ranch 1.8.1

ifeq ($(COWBOY_QUICER),1)
DEPS += quicer
dep_quicer = git https://github.com/emqx/quic main
endif

DOC_DEPS = asciideck

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper gun
dep_ct_helper = git https://github.com/extend/ct_helper master
dep_gun = git https://github.com/ninenines/gun master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-24+
AUTO_CI_WINDOWS ?= OTP-LATEST-24+

# Hex configuration.

define HEX_TARBALL_EXTRA_METADATA
#{
	licenses => [<<"ISC">>],
	links => #{
		<<"User guide">> => <<"https://ninenines.eu/docs/en/cowboy/2.13/guide/">>,
		<<"Function reference">> => <<"https://ninenines.eu/docs/en/cowboy/2.13/manual/">>,
		<<"GitHub">> => <<"https://github.com/ninenines/cowboy">>,
		<<"Sponsor">> => <<"https://github.com/sponsors/essen">>
	}
}
endef

hex_req_ranch = >= 1.8.0 and < 3.0.0
hex_req_cowlib = >= 2.14.0 and < 3.0.0

# Standard targets.

include erlang.mk

# Don't run the examples/autobahn test suites by default.

ifndef FULL
CT_SUITES := $(filter-out examples http_perf ws_autobahn ws_perf,$(CT_SUITES))
endif

# Don't run HTTP/3 test suites on Windows.

ifeq ($(PLATFORM),msys2)
CT_SUITES := $(filter-out rfc9114 rfc9204 rfc9220,$(CT_SUITES))
endif

# Compile options.

ERLC_OPTS += +warn_missing_spec +warn_untyped_record # +bin_opt_info
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

ifeq ($(COWBOY_QUICER),1)
ERLC_OPTS += -D COWBOY_QUICER=1
TEST_ERLC_OPTS += -D COWBOY_QUICER=1
endif

# Generate rebar.config on build.

app:: rebar.config

# Fix quicer compilation for HTTP/3.

autopatch-quicer::
	$(verbose) printf "%s\n" "all: ;" > $(DEPS_DIR)/quicer/c_src/Makefile.erlang.mk

# Dialyze the tests.

#DIALYZER_OPTS += --src -r test

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

# Prepare for the release.

prepare_tag:
	$(verbose) $(warning Hex metadata: $(HEX_TARBALL_EXTRA_METADATA))
	$(verbose) echo
	$(verbose) echo -n "Most recent tag:            "
	$(verbose) git tag --sort taggerdate | tail -n1
	$(verbose) git verify-tag `git tag --sort taggerdate | tail -n1`
	$(verbose) echo -n "MAKEFILE: "
	$(verbose) grep -m1 PROJECT_VERSION Makefile
	$(verbose) echo -n "APP:                 "
	$(verbose) grep -m1 vsn ebin/$(PROJECT).app | sed 's/	//g'
	$(verbose) echo -n "GUIDE:  "
	$(verbose) grep -h dep_$(PROJECT)_commit doc/src/guide/*.asciidoc || true
	$(verbose) echo
	$(verbose) echo "Links in the README:"
	$(verbose) grep http.*:// README.asciidoc
	$(verbose) echo
	$(verbose) echo "Titles in most recent CHANGELOG:"
	$(verbose) for f in `ls -rv doc/src/guide/migrating_from_*.asciidoc | head -n1`; do \
		echo $$f:; \
		grep == $$f; \
	done
	$(verbose) echo
	$(verbose) echo "Dependencies:"
	$(verbose) grep ^DEPS Makefile || echo "DEPS ="
	$(verbose) grep ^dep_ Makefile || true
	$(verbose) grep ^hex_req_ Makefile || true
	$(verbose) echo
	$(verbose) echo "rebar.config:"
	$(verbose) cat rebar.config || true
