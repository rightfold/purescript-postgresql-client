all: build test

.PHONY: build
build: src/Database/PostgreSQL/Row.purs
	pulp build

.PHONY: test
test: src/Database/PostgreSQL/Row.purs
	pulp test

src/Database/PostgreSQL/Row.purs: Rows.pl
	mkdir -p $(dir $@)
	perl Rows.pl $@
