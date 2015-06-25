
=======
[![Build Status](https://travis-ci.org/jsgosnell/ratlantis.svg?branch=master)](https://travis-ci.org/jsgosnell/ratlantis)

R code for interfacing with Atlantis ecosystem modeling software

To install development version:
```R
install.packages("devtools")
devtools::install_github("jsgosnell/ratlantis")
```

## Documentation
roxygen2 is used to generate .Rd and NAMESPACE by running:
```R
 library(roxygen2)
 roxygenize(".")
```

Vignettes are generated using ```knitr``` and ```markdown``` packages.

## Meta

Please [report any issues or bugs](https://github.com/jsgosnell/ratlantis/issues).
