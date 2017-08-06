all:
	rebar compile -v

clean:
	rebar clean

check: test
test: all
	rebar eunit -v
