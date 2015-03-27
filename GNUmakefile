
PROTO_FILES=$(wildcard test/proto-files/*.proto)

ifndef SCHEME 
$(info Please specify your implementation)
$(info "$(MAKE)" SCHEME=implementation LOAD_FLAG=load flag prefix)
$(info e.g. "(MAKE)" SCHEME=sagittarius LOAD_FLAG=-L)
$(error No SCHEME is given)
endif

# silly...
ifndef LOAD_FLAG
$(info Please specify your implementation)
$(info "$(MAKE)" SCHEME=implementation LOAD_FLAG=load flag prefix)
$(info e.g. "(MAKE)" SCHEME=sagittarius LOAD_FLAG=-L)
$(error No LOAD_FLAG is given)
endif

LIB_PATH=$(shell pwd)

.PHONY: all clean generate

all: generate test
	@echo $?

generate:
	@echo Generating libraries from $(PROTO_FILES).
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/proto-files/generate.scm $(PROTO_FILES)

clean:
	cd test/proto-files
	rm -f *.tmp
	rm -f *.sls
