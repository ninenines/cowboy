PROJECT = cookie

DEPS = cowboy erlydtl
dep_cowboy = pkg://cowboy master
dep_erlydtl = pkg://erlydtl master

.PHONY: release clean-release

release: clean-release all
	relx

clean-release:
	rm -rf _rel

include ../../erlang.mk
