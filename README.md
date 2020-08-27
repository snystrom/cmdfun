cmdfun
================

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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

## Quick Overview

The `cmdfun` framework provides mechanisms for capturing function
arguments:

  - `cmd_args_dots()` captures all arguments passed to `...`
  - `cmd_args_named()` captures all keyword arguments defined by the
    user
  - `cmd_args_all()` captures both named + dot arguments

<!-- end list -->

``` r
library(cmdfun)

myFunction <- function(input, ...){
  cmd_args_all()
}

(argsList <- myFunction(input = "test", boolean_flag = TRUE))
```

    ## $input
    ## [1] "test"
    ## 
    ## $boolean_flag
    ## [1] TRUE

`cmd_list_interp` converts the captured argument list to a parsed list
of flag/value pairs.

``` r
(flagsList <- cmd_list_interp(argsList))
```

    ## $input
    ## [1] "test"
    ## 
    ## $boolean_flag
    ## [1] ""

`cmd_list_to_flags` converts a list to a vector of commandline-style
flags using the list names as flag names and the list values as the flag
values (empty values return only the flag). This output can be directly
fed to `system2` or `processx`.

``` r
cmd_list_to_flags(flagsList)
```

    ## [1] "-input"        "test"          "-boolean_flag"

`cmd_path_search()` allows package builders to search default locations
for installed
tools.

``` r
bin_path <- cmd_path_search(default_path = "/bin", utils = c("ls", "cut"))

bin_path(util = "ls")
```

    ## [1] "//bin/ls"

## Introduction

`cmdfun` attempts to solve the problem of wrapping external software in
R. Calling external software is done with `system2` or `processx`.

For example, calling `ls -l *.md` using
    `system2`.

``` r
system2("ls", "-l *.md", stdout = TRUE)
```

    ## [1] "-rw-r--r-- 1 snystrom its_employee_psx 1077 Aug 20 19:20 LICENSE.md"      
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx  837 Aug 26 21:51 NEWS.md"         
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 7718 Aug 26 20:14 README.md"       
    ## [4] "-rw-r--r-- 1 snystrom its_employee_psx  744 Aug 26 21:35 cran-comments.md"

However, when using multiple commandline flags each flag should be
passed as a member of a character vector as follows:

When calling `ls -l
    -i`

``` r
system2("ls", c("-l", "-i", "*.md"), stdout = TRUE)
```

    ## [1] "1163031755 -rw-r--r-- 1 snystrom its_employee_psx 1077 Aug 20 19:20 LICENSE.md"      
    ## [2] "1163031757 -rw-r--r-- 1 snystrom its_employee_psx  837 Aug 26 21:51 NEWS.md"         
    ## [3] "1163031758 -rw-r--r-- 1 snystrom its_employee_psx 7718 Aug 26 20:14 README.md"       
    ## [4] "1163031762 -rw-r--r-- 1 snystrom its_employee_psx  744 Aug 26 21:35 cran-comments.md"

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

### Wrapping `ls` with cmdfun

These tools can be used to easily wrap `ls`

``` r
library(magrittr)

shell_ls <- function(dir = ".", ...){
  # grab arguments passed to "..." in a list
  flags <- cmd_args_dots() %>% 
    # prepare list for conversion to vector
    cmd_list_interp() %>% 
    # Convert the list to a flag vector
    cmd_list_to_flags()
  
  # Run ls shell command
  system2("ls", c(flags, dir), stdout = TRUE)
}
```

``` r
shell_ls("*.md")
```

    ## [1] "LICENSE.md"       "NEWS.md"          "README.md"        "cran-comments.md"

#### Boolean flags are passed as bool operators

`ls -l` can be mimicked by passing `l = TRUE` to
    ‘…’.

``` r
shell_ls("*.md", l = TRUE)
```

    ## [1] "-rw-r--r-- 1 snystrom its_employee_psx 1077 Aug 20 19:20 LICENSE.md"      
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx  837 Aug 26 21:51 NEWS.md"         
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 7718 Aug 26 20:14 README.md"       
    ## [4] "-rw-r--r-- 1 snystrom its_employee_psx  744 Aug 26 21:35 cran-comments.md"

### Named vectors can be used to provide user-friendly aliases for single-letter flags

Commandline tools can have hundreds of arguments, many with
uninformative, often single-letter, names. To prevent developers from
having to write aliased function arguments for all, often conflicting
flags, `cmd_list_interp` can additionally use a lookup table to allow
developers to provide informative function argument names for
unintuitive flags.

For example, allowing `long` to act as `-l` in `ls`.

``` r
shell_ls_alias <- function(dir = ".", ...){
  
  # Named vector acts as lookup table
  # name = function argument
  # value = flag name
  names_arg_to_flag <- c("long" = "l")
  
  flags <- cmd_args_dots() %>% 
    # Use lookup table to manage renames
    cmd_list_interp(names_arg_to_flag) %>% 
    cmd_list_to_flags()
  
  system2("ls", c(flags, dir), stdout = TRUE)
}
```

``` r
shell_ls_alias("*.md", long = TRUE)
```

    ## [1] "-rw-r--r-- 1 snystrom its_employee_psx 1077 Aug 20 19:20 LICENSE.md"      
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx  837 Aug 26 21:51 NEWS.md"         
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 7718 Aug 26 20:14 README.md"       
    ## [4] "-rw-r--r-- 1 snystrom its_employee_psx  744 Aug 26 21:35 cran-comments.md"

### Wrapping `cut` with cmdfun

Here is another example wrapping `cut` which separates text on a
delimiter (set with `-d`) and returns selected fields (set with `-f`)
from the separation.

``` r
shell_cut <- function(text, ...){

  names_arg_to_flag <- c("sep" = "d",
                         "fields" = "f")
    
    flags <- cmd_args_dots() %>%
        cmd_list_interp(names_arg_to_flag) %>% 
      cmd_list_to_flags()

    system2("cut", flags, stdout = T, input = text)
}
```

``` r
shell_cut("hello_world", fields = 2, sep = "_") 
```

    ## [1] "world"

#### Multiple values are passed as vectors

``` r
shell_cut("hello_world_hello", fields = c(1,3), sep = "_") 
```

    ## [1] "hello_hello"

Additionally, `cmdfun` provides utilites for searching & checking valid
tool installs, expecting system behavior, and helpful error handling to
allow simple construction of external tool wrappers (see
[vignette](https://snystrom.github.io/cmdfun/articles/cmdfun.html) for
details).

## More Details

See <https://snystrom.github.io/cmdfun/articles/cmdfun.html> for the
most recent documentation and to learn about all `cmdfun` features.

To file bug reports, please visit
<https://github.com/snystrom/cmdfun/issues> while providing a
[reproducible example](https://reprex.tidyverse.org/) of your issue.
