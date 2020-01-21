GHCFLAGS := $(GHCFLAGS) -O2

slides.pdf: slides.md
	pandoc -t beamer -i -o $@ $<

.PHONY: run
run: dialog ram.img
	./dialog $(if $(steps),-n $(steps),) $(netlist)

.PHONY: runclock
runclock: clock ram.img proc.net
	./clock $(if $(async),--async,) proc.net

.PHONY: dialog
dialog:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Dialog src/Dialog.hs

.PHONY: clock
clock:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Clock src/Clock.hs

.PHONY: assembler
assembler:
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ -main-is Assembler src/Assembler.hs

minijazz/mjc.byte: minijazz/analysis/*
	cd minijazz && ocamlbuild mjc.byte

proc.net: proc.mj minijazz/mjc.byte
	minijazz/mjc.byte proc.mj

ram.img: assembler clock.asm
	./assembler clock.asm > $@

.PHONY: clean
clean:
	rm -rf build dialog clock assembler ram.img rom.img proc.net marvin.tar.gz

.PHONY: archive
archive:
	git archive -v --prefix=marvin/ -o marvin.tar.gz HEAD
