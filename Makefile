all:
	rebar compile -v

check: test
test: all
	rebar eunit -v
