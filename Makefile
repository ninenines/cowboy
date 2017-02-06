# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec
COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -pa test -ct_hooks cowboy_ct_hook []
PLT_APPS = crypto public_key ssl
CI_OTP = OTP_R16B01 OTP_R16B02 OTP_R16B03-1 OTP-17.0.2 OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1 OTP-17.5.6.2 OTP-18.0.2

# Dependencies.

DEPS = cowlib ranch
dep_cowlib_commit = 1.0.2
dep_ranch_commit = 1.3.2

TEST_DEPS = ct_helper gun
dep_ct_helper = git https://github.com/extend/ct_helper.git master
dep_gun = git https://github.com/ninenines/gun b85c1f726ca49ac0e3abdcf717317cb95b06207d

# Standard targets.

include erlang.mk

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
