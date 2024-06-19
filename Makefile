
## Pipeline for AUTOML training

current: target
-include target.mk

######################################################################

Sources += $(wildcard *.Rmd *.md *.R *.bst *.bib)
Sources += presentations/*

Ignore += README.pdf README.docx

autopipeR = defined

######################################################################

### DOC conversion
%.pdf: %.md
	pandoc $< -o $@
%.docx: %.md
	pandoc $< -o $@

README.pdf: README.md
README.docx: README.md


######################################################################

simple_ml_pipeline.Rout: simple_ml_pipeline.R


######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
