dotargs
================

## A simple framework for building shell interfaces

## Install

``` r
remotes::install_github("snystrom/dotargs")
```

## Examples

``` r
library(magrittr)
library(dotargs)

shellCut <- function(text, ...){

    args <- getDots(...) %>%
        dotsToArgs

    system2("echo", c(text, "|", "cut", args))

}
```

``` r
shellCut("hello_world", f = 2, d = "_") 
```

    ## world

**WARNING:** It’s still possible to do unsafe operations as follows:

``` r
shellCut("hello_world", f = 2, d = "_", `&&echo` = "test")
```

    ## world

    ## test

### Boolean flags are passed as bool operators

``` r
shell_ls <- function(dir = ".", ...){
  args <- getDots(...) %>% 
    dotsToArgs
  
  system2("ls", c(dir, args))
}
```

``` r
shell_ls("R")
```

    ## dots_to_args.R

``` r
shell_ls("R", l = T)
```

    ## total 4

    ## -rw-r--r-- 1 snystrom employee 1372 Mar 25 11:22 dots_to_args.R

### Named vectors can be used to provide user-friendly aliases for single-letter flags

``` r
shell_ls_alias <- function(dir = ".", ...){
  
  argsDict <- c("long" = "l")
  
  args <- getDots(...) %>% 
    dotsToArgs(argsDict)
  
  system2("ls", c(dir, args))
}
```

``` r
shell_ls_alias("R", long = T)
```

    ## total 4

    ## -rw-r--r-- 1 snystrom employee 1372 Mar 25 11:22 dots_to_args.R

``` r
shellCut_alias <- function(text, ...){

  argsDict <- c("sep" = "d")
    
    args <- getDots(...) %>%
        dotsToArgs(argsDict)

    system2("echo", c(text, "|", "cut", args))
}
```

``` r
shellCut_alias("hello_world", f = 2, sep = "_") 
```

    ## world
