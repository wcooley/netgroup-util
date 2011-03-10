BINS = netgroup-graph

test:
	@for t in t_*.hs; do echo $$t ":"; runhaskell $$t; done

$(BINS):
	ghc --make $@

clean:
	-rm {,*/}*.{hi,o}

.PHONY: $(BINS) test clean
