format:
	rebar3 fmt -w

shell:
	rebar3 shell

diagrams:
	bash diagrams/build-diagrams.sh

.PHONY: format shell diagrams
