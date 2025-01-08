# ----------------------------------- #
# GNU Make Automatic Variables:
# ----------------------------------- #
# $*: stem of target
# $@: filename of target 
# $%: target member name
# $<: the first prerequisites for target
# $^: prerequisites for target
# $?: prerequisites newer than target
#######################
# Local Variables:
#######################
MV = mv -f
RM = rm -rf
CP = cp -f
ZIP = zip -r
TOUCH = touch
RSCRIPT = Rscript --vanilla
PDFLATEX = pdflatex
#########################
.PRECIOUS: %.tex Makefile
.PHONY: clean
#########################

all: clean

%.pdf: %.tex
	@ $(PDFLATEX) $*
	@ $(PDFLATEX) $*

zip:
	@ cd ..;\
	$(ZIP) reds-kaggle.zip reds-kaggle -x "reds-kaggle/.git/*" "reds-kaggle/.Rproj.user/*"

clean:
	@ $(RM) *.log *.aux *.out *.toc

