
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

all:
	@echo 'test' or 'clean' target must be specified

generate:
	@echo Generating libraries from $(PROTO_FILES).
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) \
		test/proto-files/generate.scm $(PROTO_FILES)
	@for f in $(wildcard *.tmp); do \
	    filename="$${f%.*}"; \
	    IFS=. read -a array <<< "$$filename";\
	    end=`expr $${#array[@]} - 2`; \
	    dir=tmp; \
	    for i in $$(seq 0 $$end); do \
		dir=$$dir/$${array[$$i]}; \
	    done; \
	    mkdir -p $$dir; \
	    echo "$$f to $$dir/$${array[`expr $${#array[@]} - 1`]}.sls"; \
	    mv $$f $$dir/$${array[`expr $${#array[@]} - 1`]}.sls; \
	done;

test: generate
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/compile/test-tokenize.scm
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/compile/test-parse.scm
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/compile/test-codegen.scm
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/test-compile.scm
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) test/test-private.scm
	$(SCHEME) $(LOAD_FLAG) $(LIB_PATH) $(LOAD_FLAG) tmp test/test-addressbook.scm

clean:
	cd test/proto-files
	rm -f *.tmp
	rm -f *.sls
	rm -rf tmp
