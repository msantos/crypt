.PHONY: all compile clean test dialyzer typer lint

REBAR ?= rebar3
ELVIS ?= elvis

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) xref ct

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer \
        -pa _build/default/lib/crypt/ebin \
        -I include \
        --plt _build/default/*_plt \
        -r ./src

lint:
	@$(ELVIS) rock
