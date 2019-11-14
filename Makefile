.PHONY: build run main build-test run-test test

build:
	ml-build hlc.cm Main.main main

run:
	sml @SMLload main.amd64-darwin $(source)

main: build run

build-test:
	ml-build hlc.cm Test.main test

run-test:
	sml @SMLload test.amd64-darwin $(source)

test: build-test run-test
