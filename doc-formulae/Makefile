FORMULAE=fft-formula.svg ifft-formula.svg

all: $(FORMULAE)

%.dvi: %.tex
	latex $<

%.svg: %.dvi
	dvisvgm $< -n

.PHONY: clean
clean:
	$(RM) $(FORMULAE) *.dvi *.log *.aux
