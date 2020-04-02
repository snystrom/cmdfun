dotargs
================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## A simple framework for building shell interfaces

`dotargs` provides two main functions: `getDotArgs` and `argsToFlags`
which can act as a useful backend for converting keyword arguments
passed to `...` in a function call into a vector of shell command flags
which can be passed to `system2` or `processx`. Together, they can be
used to build user-friendly R interfaces to shell programs without
having to manually implement all commandline flags in R functions.

Also `getAllArgs` and `getNamedArgs` can be used to get all named
arguments + dots, or named arguments only (no dots), depending on your
use-case.

## Install

``` r
remotes::install_github("snystrom/dotargs")
```

## Examples

``` r
library(magrittr)
library(dotargs)
```

variables defined in `...` are converted to character vector of flags
appropriate for shell commands.

``` r
myFunction <- function(...){

  flags <- getDotArgs() %>%
    argsToFlags()
  
  return(flags)
}
```

``` r
myFunction(flag = "var", bool_flag = TRUE)
```

    ## [1] "-flag var"   "-bool_flag "

This character vector can be passed to `system2` to build shell
commands.

``` r
shellCut <- function(text, ...){

  flags <- getDotArgs() %>%
    argsToFlags()

    system2("cut", flags, stdout = T, input = text)

}
```

``` r
shellCut("hello_world", f = 2, d = "_") 
```

    ## [1] "world"

Multiple values can be passed to arguments using vectors

``` r
shellCut("hello_world_hello", f = c(1,3), d = "_") 
```

    ## [1] "hello_hello"

### Boolean flags are passed as bool operators

``` r
shell_ls <- function(dir = ".", ...){
  flags <- getDotArgs() %>% 
    argsToFlags()
  
  system2("ls", c(dir, flags), stdout = T)
}
```

``` r
shell_ls("R")
```

    ## [1] "dots_to_args.R" "utils.R"

``` r
shell_ls("R", l = T)
```

    ## [1] "total 16"                                                               
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx 4115 Apr  2 18:06 dots_to_args.R"
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 6217 Apr  2 18:08 utils.R"

### Named vectors can be used to provide user-friendly aliases for single-letter flags

``` r
shell_ls_alias <- function(dir = ".", ...){
  
  argsDict <- c("long" = "l")
  
  flags <- getDotArgs() %>% 
    argsToFlags(argsDict)
  
  system2("ls", c(dir, flags), stdout = T)
}
```

``` r
shell_ls_alias("R", long = T)
```

    ## [1] "total 16"                                                               
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx 4115 Apr  2 18:06 dots_to_args.R"
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 6217 Apr  2 18:08 utils.R"

``` r
shellCut_alias <- function(text, ...){

  argsDict <- c("sep" = "d")
    
    flags <- getDotArgs() %>%
        argsToFlags(argsDict)

    system2("cut", flags, stdout = T, input = text)
}
```

``` r
shellCut_alias("hello_world", f = 2, sep = "_") 
```

    ## [1] "world"

## Unsafe operations

**WARNING:** It’s still possible to do unsafe operations as follows, so
please be careful how you build system calls.

``` r
shellCut_unsafe <- function(text, ...){

  flags <- getDotArgs() %>%
    argsToFlags()

    system2("echo", c(text , "|", "cut", flags), stdout = T)

}

shellCut_unsafe("hello_world", f = 2, d = "_ && echo test")
```

    ## [1] "world" "test"

**NOTE** even if when setting `stdout = TRUE` the second command doesn’t
appear in the output, it will still have run.

A more extreme example of what can happen is here, where
`~/deleteme.txt` will be removed silently.

I promise I’ll get around to sanitizing user input eventually.

``` r
shellCut("hello_world", f = 2, d = "_ && rm ~/deleteme.txt")
```
