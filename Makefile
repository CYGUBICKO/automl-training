
## Pipeline for AUTOML training

current: target
-include target.mk

######################################################################

Sources += $(wildcard *.Rmd *.md *.R *.bst *.bib)
Sources += data/*

Ignore += presentations/*
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

Ignore += api_config.ini
Ignore += automl-pipeline/*
Ignore += draft_report.docx
Ignore += prediction_template.csv

knitbeamer = Rscript -e "library(rmarkdown); render(\"$<\", beamer_presentation())"

######################################################################

simple_ml_pipeline.Rout: simple_ml_pipeline.R
upload_data.Rout: upload_data.R
model_config.Rout: model_config.R
Sources += models_hyperparameters.xlsx 
models_hyperparameters.Rout: models_hyperparameters.R

funs.Rout: funs.R
draft_report.pdf: draft_report.Rmd
	$(knitpdf)

######################################################################

## Presentation
Ignore += machine_learning_concepts.Rmd
Ignore += machine_learning_concepts.pdf
machine_learning_concepts.pdf: machine_learning_concepts.Rmd
	$(knitbeamer)

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
