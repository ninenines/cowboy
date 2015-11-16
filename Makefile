# See LICENSE for licensing information.

PROJECT = cowboy
PROJECT_DESCRIPTION = Small, fast, modular HTTP server.
PROJECT_VERSION = 2.0.0-pre.2
PROJECT_REGISTERED = cowboy_clock

# Options.

COMPILE_FIRST = cowboy_middleware cowboy_stream cowboy_sub_protocol
PLT_APPS = public_key ssl
CT_OPTS += -ct_hooks cowboy_ct_hook [] # -boot start_sasl
CI_OTP = OTP-18.0.3 OTP-18.1

# Dependencies.

LOCAL_DEPS = crypto

DEPS = cowlib ranch
dep_cowlib = git https://github.com/ninenines/cowlib master
dep_ranch = git https://github.com/ninenines/ranch 1.1.0

TEST_DEPS = ct_helper gun
dep_ct_helper = git https://github.com/extend/ct_helper master
dep_gun = git https://github.com/ninenines/gun master

# Standard targets.

include erlang.mk

# Compile options.

ERLC_OPTS += +warn_export_all +warn_missing_spec +warn_untyped_record
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

# Generate rebar.config on build.

app:: rebar.config

# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release.

ci-setup:: clean deps test-deps
	$(gen_verbose) cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/

# Documentation.

dep_ezdoc = git https://github.com/ninenines/ezdoc master
$(eval $(call dep_target,ezdoc))

build-doc-deps: $(DEPS_DIR)/ezdoc
	$(MAKE) -C $(DEPS_DIR)/ezdoc

define ezdoc_script
io:format("Building manual~n"),
[begin
	AST = ezdoc:parse_file(F),
	BF = filename:rootname(filename:basename(F)),
	io:format("  ~s~n", [BF]),
	file:write_file("doc/markdown/manual/" ++ BF ++ ".md", ezdoc_markdown:export(AST)),
	case BF of
		"cowboy" ++ _ when BF =/= "cowboy_app" ->
			file:write_file("doc/man3/" ++ BF ++ ".3", ezdoc_man:export(3, AST));
		_ when BF =/= "index" ->
			file:write_file("doc/man7/" ++ BF ++ ".7", ezdoc_man:export(7, AST));
		_ ->
			ok
	end
end || F <- filelib:wildcard("doc/src/manual/*.ezdoc")],
io:format("Building guide~n"),
[begin
	AST = ezdoc:parse_file(F),
	BF = filename:rootname(filename:basename(F)),
	io:format("  ~s~n", [BF]),
	file:write_file("doc/markdown/guide/" ++ BF ++ ".md", ezdoc_markdown:export(AST))
end || F <- filelib:wildcard("doc/src/guide/*.ezdoc")],
io:format("Done.~n"),
init:stop().
endef
export ezdoc_script

docs:: clean-docs build-doc-deps
	@mkdir -p doc/man3 doc/man7 doc/markdown/guide doc/markdown/manual
	$(gen_verbose) erl -noinput -pa ebin deps/ezdoc/ebin -eval "$$ezdoc_script"
	@gzip doc/man3/*.3 doc/man7/*.7
	@cp doc/src/guide/*.png doc/markdown/guide

clean-docs:
	$(gen_verbose) rm -rf doc/man3 doc/man7 doc/markdown

MAN_INSTALL_PATH ?= /usr/local/share/man

install-docs:
	mkdir -p $(MAN_INSTALL_PATH)/man3/ $(MAN_INSTALL_PATH)/man7/
	install -g 0 -o 0 -m 0644 doc/man3/*.gz $(MAN_INSTALL_PATH)/man3/
	install -g 0 -o 0 -m 0644 doc/man7/*.gz $(MAN_INSTALL_PATH)/man7/
