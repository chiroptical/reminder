format:
	rebar3 fmt -w

shell:
	rebar3 shell

diagrams:
	bash diagrams/build-diagrams.sh

lint:
	rebar3 dialyzer

.PHONY: format shell diagrams lint
