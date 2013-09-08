PROJECT = rest_stream_response

DEPS = cowboy
dep_cowboy = pkg://cowboy master

.PHONY: release clean-release

release: clean-release all
	relx

clean-release:
	rm -rf _rel

include ../../erlang.mk
