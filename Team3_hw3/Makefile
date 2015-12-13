all: hw3.html boroughs.json

hw3.html: hw3.Rmd 
	Rscript -e "library(rmarkdown);render('hw3.Rmd')"

data/merged_data.Rdata: data_management.R
	R --no-save < data_management.R

boroughs.json: compute_boundries.R
	R --no-save < compute_boundries.R

compute_boundries.R: data/merged_data.Rdata

clean:
	rm -rf data/
	rm -f hw3.html

.PHONY: all clean
