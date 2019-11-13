simulator:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ --make src/Simulator.hs

.PHONY: clean
clean:
	rm -rf build simulator
