PYTHON = ./bin/python
CQLSH = ./bin/cqlsh


.PHONY:
	all
	compile
	cassandra
	virtualevn
	python_req


deps:
	@rebar get-deps

all: compile

compile:
	@rebar compile

cassandra: python_req
	@. bin/activate && $(CQLSH) < ./files/cassandra.cql

virtualevn:
	@virtualenv -q .

python_req: virtualevn
	@. bin/activate && pip -q install -r requirements.txt

console:
	erl -pa ebin -pz deps/*/ebin
