cmdfun
================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/snystrom/cmdfun/workflows/R-CMD-check/badge.svg)](https://github.com/snystrom/cmdfun/actions)
[![Codecov test
coverage](https://codecov.io/gh/snystrom/cmdfun/branch/master/graph/badge.svg)](https://codecov.io/gh/snystrom/cmdfun?branch=master)
<!-- badges: end -->

## A simple framework for building shell interfaces

The purpose of `cmdfun` is to significantly reduce the overhead involved
in wrapping shell programs in R. The tools are intended to be intuitive
and lightweight enough to use for data scientists trying to get things
done quickly, but robust and full-fledged enough for developers to
extend them to more advanced use cases.

## Installation

Install the development version of `cmdfun` with:

``` r
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("snystrom/cmdfun")
```

## Quick Examples

`cmdfun` attempts to solve the problem of wrapping external software in
R. Calling external software is done with `system2` or `processx`.

For example, calling `ls
    -l`.

``` r
system2("ls", "-l", stdout = TRUE)
```

    ## [1] "-rw-r--r-- 1 snystrom its_employee_psx  343 Aug 26 11:12 cmdr.Rproj" 
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx  232 Aug 18 15:42 codecov.yml"
    ## [3] "drwxr-xr-x 2 snystrom its_employee_psx 4096 Aug 24 19:31 man"        
    ## [4] "drwxr-xr-x 3 snystrom its_employee_psx 4096 Jul 30 22:24 tests"      
    ## [5] "drwxr-xr-x 2 snystrom its_employee_psx 4096 Aug 25 18:33 vignettes"

However, when using multiple commandline flags each flag must be passed
as a member of a character vector as follows:

When calling `ls -l
    -a`

``` r
system2("ls", c("-l", "-i"), stdout = TRUE)
```

    ## [1] "1524844666 -rw-r--r-- 1 snystrom its_employee_psx  343 Aug 26 11:12 cmdr.Rproj" 
    ## [2] "1192376129 -rw-r--r-- 1 snystrom its_employee_psx  232 Aug 18 15:42 codecov.yml"
    ## [3] "1524844667 drwxr-xr-x 2 snystrom its_employee_psx 4096 Aug 24 19:31 man"        
    ## [4] "1484110934 drwxr-xr-x 3 snystrom its_employee_psx 4096 Jul 30 22:24 tests"      
    ## [5] "1484110945 drwxr-xr-x 2 snystrom its_employee_psx 4096 Aug 25 18:33 vignettes"

This becomes even more difficult if trying to support user input, as a
significant amount of overhead is required to parse user inputs and
optional flags into these vectors.

`cmdfun` provides utilities for converting **function arguments** into
**lists** which can easily convert to **character vectors** suitable for
use with `system2` or `processx`.

``` r
library(cmdfun)

myFunction <- function(input, option1){
  # Grabs named arguments as key/value pairs
  cmd_args_named()
}

(argsList <- myFunction("myInput.txt", "foo"))
```

    ## $input
    ## [1] "myInput.txt"
    ## 
    ## $option1
    ## [1] "foo"

``` r
# Converts list to character vector of flags & values
cmd_list_to_flags(argsList)
```

    ## [1] "-input"      "myInput.txt" "-option1"    "foo"

Additionally, `cmdfun` provides utilites for searching & checking valid
tool installs, expecting system behavior, and helpful error handling to
allow simple construction of external tool wrappers.

## More Details

See <https://snystrom.github.io/cmdfun/articles/cmdfun.html> for the
most recent documentation and to learn about all `cmdfun` features.

To file bug reports, please visit
<https://github.com/snystrom/cmdfun/issues> while providing a
[reproducible example](https://reprex.tidyverse.org/) of your issue.
