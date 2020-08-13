
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lhmetools

<!-- badges: start -->

<!-- badges: end -->

The goal of `{lhmetools}` is to bundle all generic functions frequently
used in LHMET.

# System requirements

To extract files from a `rar` archive we have created the function
`unrar()`. This function allows extract files using `unrar`\[1\] tool
from the command line. `unrar` tool can be installed using your Linux
distributions’ package manager.

    $ sudo apt update && sudo apt install --assume-yes unrar # Ubuntu and Debian

## Installation

You can install lhmetools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lhmet/lhmetools")
```

## List of funcions

  - `unrar()`: extract files from a rar arquive.

This is a basic example which shows you how to extract files:

``` r
library(lhmetools)
## basic example code
rarfile_url <- "https://ndownloader.figshare.com/files/13366451"
dest_file <- tempfile(fileext = ".rar")
download.file(rarfile_url, dest_file, mode = 'wb')
(extracted_files <- unrar(dest_file))
```

1.  It’s developed by [RARLAB](https://www.rarlab.com/download.htm) and
    made available in Linux and other Unix based operating systems.
