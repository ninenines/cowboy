#!/bin/sh

KERL_INSTALL_PATH=~/erlang
KERL_RELEASES="r15b01 r15b02 r15b03 r16b r16b01 r16b02 r16b03-1 17.0 17.0_native maint master"

make build-tests

for rel in $KERL_RELEASES
do
	echo
	echo "    TESTING $rel"
	echo
	. $KERL_INSTALL_PATH/$rel/activate
	cp ~/.kerl/builds/$rel/otp_src_*/lib/ssl/test/erl_make_certs.erl \
		deps/ct_helper/src/
	CT_OPTS="-label $rel" make tests
done

xdg-open logs/all_runs.html
