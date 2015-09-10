REBAR := rebar
.DEFAULT_GOAL = start

start: get-deps compile erl

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

erl:
	erl -pa ebin -pa ./deps/*/ebin -boot start_sasl
