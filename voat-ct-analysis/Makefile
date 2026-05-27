# Makefile — Command-line shortcuts for the Voat CT pipeline

.PHONY: all init pipeline reports shiny clean docker-build docker-run test

all: init pipeline reports

init:
	Rscript init.R

pipeline:
	Rscript -e "targets::tar_make()"

reports:
	Rscript -e "targets::tar_make(c('main_report', 'supplementary_report'))"

shiny:
	Rscript -e "shiny::runApp('shiny/app.R', port = 3838, host = '0.0.0.0')"

clean:
	Rscript -e "targets::tar_destroy()"
	rm -f reports/*.html reports/*.pdf

docker-build:
	docker build -t voat-ct-analysis .

docker-run:
	docker run --rm -p 3838:3838 -v $$(PWD)/data:/app/data voat-ct-analysis

test:
	Rscript -e "targets::tar_make(c('descriptives_table', 'validation_features_pca'))"
