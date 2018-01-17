.PHONY: deps
deps:
	npm install
	cd ./server/ && \
		stack setup && \
		stack build --only-dependencies
	cd ./frontend/ && \
		npm install && \
		bower install

.PHONY: init_local_db
init_local_db:
	sh init_db/init_local_db.sh

.PHONY: server
server:
	cd ./server/ && \
		npx env-cmd ../env/test_remote_db stack build --exec ywp

.PHONY: static
static-server:
	cd ./frontend/ && \
		npx pulp server

.PHONY: start
start:
	npx concurrently -r 'make server' 'make static-server'
