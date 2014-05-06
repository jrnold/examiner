all: README.md

%.md: %.Rmd
	Rscript -e 'library(knitr);knit("$<", output="$@")'
