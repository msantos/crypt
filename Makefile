REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

.PHONY: test dialyzer typer clean

test:
	@$(REBAR) xref ct

dialyzer:
	@$(REBAR) dialyzer
