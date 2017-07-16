all:
	rebar compile -v

check: test
test: all
	@echo TODO
