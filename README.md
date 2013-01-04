extremevalues
=============

An R-package for distribution-based outlier detection

This project is a rewrite of the [extremevalues](http://cran.r-project.org/web/packages/extremevalues/index.html) 
package. It used to be developed [here](http://code.google.com/p/extremevalues/), but I am moving it to github. 

The redeveloped package aims to be _faster_ by redeveloping someof the algorithms in C and more _maintainable_ by
better code structuring (also by using more modern tools like Roxygen2 and testthat).

Besides that I am planning to add
- support for general distributions (in stead of the five distributions in the old package)
- A shiny-based GUI (in stead of the ugly tcl-based old package)
- support for ff and laf-objects



This project is still in its infancy, but expect updates in the coming time.
