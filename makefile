BUILD=stack build

TEST=stack test

build:
	${BUILD}

build-watch:
	${BUILD} --file-watch

test :
	${TEST}

test-watch :
	${TEST} --file-watch
