#!/bin/bash

set -ux

PAPER=wqe

Rscript -e "rmarkdown::render('$PAPER.Rmd')"

if [ "${1:-}" == "--publish" ]; then
  pdflatex $PAPER.tex
  bibtex $PAPER.aux
  Rscript -e "rmarkdown::render('$PAPER.Rmd')"

  if [ -d "publish" ]; then
    rm -rf publish
  fi
  mkdir publish
  cp $PAPER.tex ./publish/
  cp $PAPER.bbl ./publish/
  cp -r ${PAPER}_files ./publish/
fi