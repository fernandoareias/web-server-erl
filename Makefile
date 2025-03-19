.PHONY: all compile clean test shell release run docs check

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean
	./scripts/clean.sh -a

test:
	rebar3 eunit
	rebar3 ct

shell:
	rebar3 shell

release:
	./scripts/release.sh -c -t

run: compile
	rebar3 shell

docs:
	rebar3 edoc

check:
	./scripts/check_style.sh

help:
	@echo "Targets:"
	@echo "  all       - Compile the project"
	@echo "  compile   - Compile the project"
	@echo "  clean     - Clean all generated files"
	@echo "  test      - Run tests"
	@echo "  shell     - Start an Erlang shell with the project loaded"
	@echo "  release   - Create a release"
	@echo "  run       - Compile and run the project"
	@echo "  docs      - Generate documentation"
	@echo "  check     - Check code style"
	@echo "  help      - Show this help message"