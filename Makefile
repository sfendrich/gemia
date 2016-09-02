FIGS=global r r12
FIGS_DOT=$(FIGS:%=%.dot)
FIGS_PDF=$(FIGS:%=%.pdf)


.PHONY:
doc: gemia.pdf

$(FIGS_DOT): ex-dataserver-gemia.lhs
	echo $(FIGS_DOT)
	runhaskell $< $(FIGS)

%.pdf: %.dot
	dot -Tpdf -Gmargin=0 $^ >$@
	
gemia.pdf: ex-dataserver-gemia.lhs $(FIGS_PDF)
	pdflatex -jobname gemia $^

.PHONY:
clean:
	$(RM) gemia.aux gemia.log gemia.pdf $(FIGS_DOT) $(FIGS_PDF)
