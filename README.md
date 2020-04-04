dotargs
================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
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

    ## [1] "dots_to_args.R"   "macros.R"         "utils_internal.R" "utils.R"

``` r
shell_ls("R", l = T)
```

    ## [1] "total 28"                                                                 
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx 4548 Apr  4 00:37 dots_to_args.R"  
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 7635 Apr  4 17:45 macros.R"        
    ## [4] "-rw-r--r-- 1 snystrom its_employee_psx 6249 Apr  4 17:46 utils_internal.R"
    ## [5] "-rw-r--r-- 1 snystrom its_employee_psx 1418 Apr  4 00:37 utils.R"

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

    ## [1] "total 28"                                                                 
    ## [2] "-rw-r--r-- 1 snystrom its_employee_psx 4548 Apr  4 00:37 dots_to_args.R"  
    ## [3] "-rw-r--r-- 1 snystrom its_employee_psx 7635 Apr  4 17:45 macros.R"        
    ## [4] "-rw-r--r-- 1 snystrom its_employee_psx 6249 Apr  4 17:46 utils_internal.R"
    ## [5] "-rw-r--r-- 1 snystrom its_employee_psx 1418 Apr  4 00:37 utils.R"

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

## Abstraction of command path handling

A common pattern when designing shell interfaces is to ask the user to
give an absolute path to the target shell utility. It is common to pass
this information from the user to R by using either R environment
variables defined in .Renviron, using options (set with `option()`, and
got with `getOption()`), having the user explicitly pass the path in the
function call, or failing this, using a default install path.

`build_path_handler()` returns a function that correctly returns the
path to the target utility.

For example, to build an interface to the “MEME” suite, which is by
default installed to “~/meme/bin”, one could build the following:

``` r
handle_meme_path <- build_path_handler(default_path = "~/meme/bin")

handle_meme_path()
```

    ## [1] "/nas/longleaf/home/snystrom/meme/bin"

To only search the R environment variable “MEME\_PATH”, one could build:

``` r
handle_meme_path <- build_path_handler(environment_var = "MEME_PATH")
```

``` r
# Without environment varialbe defined
handle_meme_path()
```

    ## Error in handle_meme_path(): No path defined or detected

``` r
# With environment varialbe defined
Sys.setenv("MEME_PATH" = "~/meme/bin")
handle_meme_path()
```

    ## [1] "/nas/longleaf/home/snystrom/meme/bin"

Multiple arguments can be used, and they will be searched from
most-specific, to most-general.

``` r
handle_meme_path <- build_path_handler(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin")
```

For example, if “MEME\_PATH” is invalid on my machine, the handler will
return the default path as long as the default is also valid on my
machine.

``` r
Sys.setenv("MEME_PATH" = "bad/path")
handle_meme_path()
```

    ## [1] "/nas/longleaf/home/snystrom/meme/bin"

### Support for tool utilities

Some software, like the MEME suite is distributed as several binaries
located in a common directory. To allow interface builders to officially
support specific binaries, each binary can be defined as a “utility”
within the build path.

Here, I will include two tools from the MEME suite, AME, and DREME
(distributed as binaries named “ame”, and “dreme”).

``` r
handle_meme_path <- build_path_handler(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin",
                                       utils = c("dreme", "ame"))
```

handler functions have two optional arguments: `path` and `util`. `path`
acts as an override to the defaults provided when building the handler.
User-provided path variables will always be used instead of provided
defaults. This is to catch problems from the user and not cause
unexpected user-level
    behavior.

``` r
handle_meme_path("bad/path")
```

    ## Error in check_valid_command_path(path): Command: bad/path, does not exist.

`util` specifies which utility path to return (if any).

``` r
handle_meme_path(util = "dreme")
```

    ## [1] "/nas/longleaf/home/snystrom/meme/bin/dreme"

## Bringing it all together

Using a `get*Args()` family function to get and convert function
arguments to commandline flags. The path handler returns the correct
`command` call which can be passed to `system2` or `processx` along with
the flags generated from `argsToFlags`.

This makes for a robust shell wrapper without excess overhead.

``` r
handle_meme_path <- build_path_handler(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin",
                                       utils = c("dreme", "ame"))

runDreme <- function(..., meme_path = NULL){
  flags <- getDotArgs() %>% 
    argsToFlags()
  
  command <- handle_meme_path(path = meme_path, util = "dreme")
  
  system2(command, flags)
}
```

Commands can now run through `runDreme` by passing flags as function
arguments.

``` r
runDreme(h = T)
```

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
