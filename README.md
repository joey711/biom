<link href="http://joey711.github.com/phyloseq/markdown.css" rel="stylesheet"></link>

BIOM format in R
=======================

This is an R package for interfacing with the [BIOM](http://biom-format.org/) file format. This package includes basic tools for reading biom-format files, accessing and subsetting data tables from a biom object, as well as limited support for writing a biom-object back to a biom-format file. The design of this API is intended to match the python API and other tools included with the biom-format project, but with a decidedly "R flavor" that should be familiar to R users. This includes S4 classes and methods, as well as extensions of common core functions/methods.

To install the latest stable release of the biom package enter the following command from within an R session:

```S
install.packages("biom")
```

To install the latest development version of the biom package, enter the following lines in an R session:

```S
install.packages("devtools") # if not already installed
library("devtools")
install_github("biom", "joey711")
```

 * Please post feature or support requests and bugs at the [issues tracker for the biom package](https://github.com/joey711/biom/issues) on GitHub. Issues related to the format itself and not the R interface should be posted on the [issues tracker for the biom format](https://github.com/biom-format/biom-format/issues).
 
 * Note that the licenses between the biom R package (GPL-2) and some of the other biom-format software are different.

 * The offical release version of this package is [made available through CRAN](http://cran.r-project.org/web/packages/biom/index.html).

