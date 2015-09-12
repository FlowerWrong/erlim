REBAR := rebar
.DEFAULT_GOAL = start

start: pre-compile get-deps compile erl

pre-compile:
	rm erlim.toml
	cp erlim.example.toml erlim.toml

get-deps:
	$(REBAR) get-deps

test:
	$(REBAR) compile eunit

compile:
	$(REBAR) compile

erl:
	erl -pa ebin -pa ./deps/*/ebin -boot start_sasl
