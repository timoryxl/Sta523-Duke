all: hw5.html

hw5.html: hw5.Rmd task1.Rdata task2.Rdata task3.Rdata
	Rscript -e "library(rmarkdown);render('hw5.Rmd')"

task1.Rdata: reddit.jan.Rdata reddit.feb.Rdata reddit.mar.Rdata reddit.apr.Rdata reddit.may.Rdata
	R --no-save < task1code.R

reddit.jan.Rdata: runTask1.R
	R --no-save < runTask1.R

reddit.feb.Rdata: runTask1.R
	R --no-save < runTask1.R

reddit.mar.Rdata: runTask1.R
	R --no-save < runTask1.R

reddit.apr.Rdata: runTask1.R
	R --no-save < runTask1.R

reddit.may.Rdata: runTask1.R
	R --no-save < runTask1.R


task2.Rdata: runTask2.R

task3.Rdata: time.gzip
	R --no-save < task3code.R

time.gzip: t3_hw5_main.R
	R --no-save < t3_hw5_main.R

.PHONY: all

