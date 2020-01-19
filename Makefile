GHCFLAGS := $(GHCFLAGS) -O2

.PHONY: run
run: simulator ram.bin proc.net
	./simulator $(if $(STEPS),-n $(STEPS),) proc.net

.PHONY: runclock
runclock: clock ram.bin proc.net
	./clock $(if $(ASYNC),--async,) proc.net

.PHONY: simulator
simulator:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Simulator src/Simulator.hs

.PHONY: clock
clock:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Clock src/Clock.hs

.PHONY: assembler
assembler:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Assembler src/Assembler.hs

proc.net: proc.mj
	minijazz/mjc.byte $^

ram.bin: assembler clock.asm
	./assembler clock.asm > $@

.PHONY: clean
clean:
	rm -rf build simulator clock assembler ram.bin rom.bin proc.net

.PHONY: archive
archive:
	tar -cvzf scapin.tar.gz src Makefile README.md proc.mj clock.asm
