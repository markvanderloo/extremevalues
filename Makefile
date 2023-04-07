pkg:
	rm -f *.tar.gz
	R CMD build pkg

check: 
	rm -f *.tar.gz
	R CMD build cran
	R CMD check *.tar.gz

cran: 
	rm -f *.tar.gz
	R CMD build --compact-vignettes="gs+qpdf" ./cran
	R CMD check --as-cran *.tar.gz

install: 
	rm -f *.tar.gz
	R CMD build cran
	R CMD INSTALL *.tar.gz


manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./cran



clean:
	rm -f *.tar.gz
	rm -rf extremevalues.Rcheck
	rm -rf manual.pdf

