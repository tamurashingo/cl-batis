
.DEFAULT_GOAL := test

.PHONY: setup test.prev test test.down test.console test.swank
setup:
	docker build -t batis-test .

test.prev:
	docker-compose down || true
	docker-compose -f docker-compose.test-runner.yml run --rm --entrypoint rm batis-test -rf ./volumes
#	rm -rf ./volumes
	mkdir ./volumes
	mkdir ./volumes/mysql
	mkdir ./volumes/postgresql
	mkdir ./volumes/postgresql/data
	mkdir ./volumes/postgresql/log
	sleep 1
	docker-compose up -d
	echo wait...
	sleep 10

test: test.prev
	@echo "Running tests..."
	docker-compose -f docker-compose.test-runner.yml run --rm --entrypoint qlot batis-test install
	docker-compose -f docker-compose.test-runner.yml run --rm --entrypoint qlot batis-test exec rove batis-test.asd

test.down:
	docker-compose down


test.console:
	docker-compose -f docker-compose.test-runner.yml run -it -p 4005:4005 --rm --entrypoint /bin/bash batis-test

test.swank:
	docker-compose -f docker-compose.test-runner.yml run -it -p 4005:4005 --rm --entrypoint ros batis-test run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
