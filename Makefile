GHCFLAGS := $(GHCFLAGS) -O2

.PHONY: run
run: dialog ram.img
	./dialog $(if $(STEPS),-n $(STEPS),) $(NETLIST)

.PHONY: runclock
runclock: clock ram.img proc.net
	./clock $(if $(ASYNC),--async,) proc.net

.PHONY: dialog
dialog:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Dialog src/Dialog.hs

.PHONY: clock
clock:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Clock src/Clock.hs

.PHONY: assembler
assembler:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Assembler src/Assembler.hs

minijazz/mjc.byte:
	cd minijazz && ocamlbuild mjc.byte

proc.net: proc.mj minijazz/mjc.byte
	minijazz/mjc.byte proc.mj

ram.img: assembler clock.asm
	./assembler clock.asm > $@

.PHONY: clean
clean:
	rm -rf build dialog clock assembler ram.img rom.img proc.net

.PHONY: archive
archive:
	tar -cvzf scapin.tar.gz src minijazz Makefile README.md proc.mj proc.net clock.asm
