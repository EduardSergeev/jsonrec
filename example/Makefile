ERLS = $(wildcard *.erl)
BEAMS = $(ERLS:%.erl=%.beam) 

.PHONY: all clean jsonrec_lib

all: $(BEAMS)

%.beam: %.erl jsonrec_lib
	@echo Compiling $<
	@erlc -pz ../ebin -pz ../deps/meta/ebin -pa . -I ../include $<

jsonrec_lib:
	@echo Building meta library
	@cd .. && rebar compile

clean:
	@echo Cleaning
	@rm -f *.beam