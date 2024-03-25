[![R-CMD-check](https://github.com/bhklab/CoreGx/workflows/R-CMD-check-bioc-devel/badge.svg)](https://github.com/bhklab/CoreGx/actions)

CoreGx
==========

Abstracted classes and functions to be inherited and extended by the PharmacoGx, RadioGx and ToxicoGx R packages.


Dependencies:

All dependencies are available from CRAN or Bioconductor.


# Install from github

```r
devtools::install_github("bhklab/CoreGx")
```

Development version:

```r
devtools::install_github("bhklab/CoreGx", ref = "devel")
```

# Install from Bioconductor

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("CoreGx")
```