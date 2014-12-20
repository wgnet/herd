compile:
	rebar compile skip_deps=true

get-deps:
	rebar get-deps

compile_all:
	rebar compile

eunit:
	rebar eunit skip_deps=true

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

clean_all:
	rebar clean
	rm -f erl_crash.dump

run:
	erl -pa ebin +pc unicode

d:
	dialyzer --src \
	-I include -r src test \
	| fgrep --invert-match --file .dialyzer.ignore

etags:
	etags src/* include/*
