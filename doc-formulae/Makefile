FORMULAE=fft-formula.svg ifft-formula.svg

all: $(FORMULAE)

%.dvi: %.tex
	cat texhead.tex $< texfoot.tex > tmp_$<
	latex tmp_$<
	mv $(addsuffix .dvi,$(basename tmp_$<)) $@
	$(RM) $(addsuffix .*,$(basename tmp_$<))

%.svg: %.dvi
	dvisvgm $< -n

.PHONY: clean
clean:
	$(RM) $(FORMULAE) *.dvi *.log *.aux
