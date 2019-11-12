TARGET   = simulator

GHCFLAGS = -dynamic

.PHONY: $(TARGET)
$(TARGET):
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ --make src/Main.hs

.PHONY: clean
clean:
	rm -rf build $(TARGET)
