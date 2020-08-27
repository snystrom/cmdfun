#' Return all named arguments and arguments passed as dots from parent function call
#'
#' @param keep name of arguments to keep
#' @param drop name of arguments to drop (NOTE: keep or drop are mutually exclusive settings)
#' 
#' @return named list of all arguments passed to parent
#' @export
#'
#' @examples
#' theFunction <- function(arg1, ...) { cmd_args_all() }
#' theArgs <-  theFunction(arg1 = "test", example = "hello")
cmd_args_all <- function(keep = NULL, drop = NULL){
  # Modified from:
  # https://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-function-in-r-with-names
  # using callstack position of parent call will always evaluate to function
  # this was called inside, this allows this to work inside pipes
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     #envir = parent.frame(),
                     expand.dots = TRUE))[-1]
  
  # arguments from callstack need to be evaluated in parent environment.
  # eval's default is to execute in the parent frame of where it was called,
  # which in this case would be **within** the lapply loop. 
  # setting envir = parent.frame() will evaluate **before** the loop, so the
  # environment will be the same as the match.call parent as intended.
  args <- lapply(argList, eval, envir = parent.frame())
  
  list_keep_or_drop(args, keep = keep, drop = drop)
  
}


#' return function dots from parent function as named list
#'
#'
#' @param keep name of arguments to keep
#' @param drop name of arguments to drop (NOTE: keep or drop are mutually exclusive settings)
#'
#' @return named list of kwargs from ...
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' theFunction <- function(...) { cmd_args_dots() }
#' theDots <-  theFunction(example = "hello", boolFlag = TRUE, vectorFlag = c(1,2,3))
cmd_args_dots <- function(keep = NULL, drop = NULL){
  
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     expand.dots = FALSE))[-1]
  
  args <- lapply(argList[["..."]], eval, envir = parent.frame()) 
  
  list_keep_or_drop(args, keep = keep, drop = drop)
}

#' Return all named arguments from parent function call
#' 
#' @param keep name of arguments to keep
#' @param drop name of arguments to drop (NOTE: keep or drop are mutually exclusive settings)
#'
#' @return named list of all defined function arguments from parent
#' @export
#'
#' @examples
#' theFunction <- function(arg1, ...) { cmd_args_named() }
#' theNamedArgs <-  theFunction(arg1 = "test", example = "hello")
cmd_args_named <- function(keep = NULL, drop = NULL){
  # see getAllNamedArgs for explanation of how this chunk works
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     expand.dots = FALSE))[-1]
  
  args <- lapply(argList, eval, envir = parent.frame()) %>% 
    cmd_list_drop_named("...")
  
  list_keep_or_drop(args, keep = keep, drop = drop)
}

#' Convert list of function arguments to list of command flags
#'
#' Function also handles error checking to ensure args contain valid data types,
#' and looks for common usage mistakes.
#' 
#' The list structure is more amenable to manipulation by package developers for
#' advanced use before evaluating them to the command flags vector with
#' cmd_list_to_flags().
#'
#' @param args named list output from get*Args family of functions.
#' @param flag_lookup optional named vector used to convert args to command flags
#'
#' @return named list 
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' 
#' @examples
#' theFunction <- function(...){cmd_args_all()}
#' theArgs <- theFunction(arg1 = "value", arg2 = TRUE)
#' flagList <- cmd_list_interp(theArgs)
#' flags <- cmd_list_to_flags(flagList)
cmd_list_interp <- function(args, flag_lookup = NULL){
  
  testthat::expect_type(args, "list")
  
  if (length(args) == 0) { return(NULL) }
  
  testthat::expect_named(args)
  
  if (any(flag_lookup == "TRUE" | flag_lookup == "FALSE")) {
    warning("flag_lookup may contain boolean definitions, which could cause unexpected behavior")
  }
  
  if (!is.null(flag_lookup)) {
    args <- convert_names(args, flag_lookup)
  }
  
  if (is.null(flag_lookup)) { 
    flag_lookup <- args_as_lookup(args)
  }
  
  # Check for illegal characters in args, print warning
  check_args_contain_illegal_flags(args)

  args %<>% 
    # NULL flags should be removed
    drop_list_NULL() %>% 
    # NA flags should be removed
    purrr::discard(~{all(is.na(.x))}) %>% 
    # collapse logicals, T = include, replace for empty string
    true_to_empty() %>% 
    # only FALSE logicals remain, so they are dropped
    drop_list_logicals() %>% 
    # Remove anything with empty names (happens )
    cmd_list_drop_named("")
  
  # Warn if arguments are defined multiple times
  purrr::imap_dbl(flag_lookup, count_matched_args, args) %>% 
    purrr::set_names(concatenate_args(flag_lookup)) %>% 
    find_multimatched_args() %>% 
    purrr::walk(warn_multimatched_arg)
  
  if (length(args) == 0) {return(NULL)}

  return(args)
}

#' Convert flag list to vector of command flags
#'
#' @param flagList output from cmd_list_interp(). A named list where names
#'   correspond to flags and members correspond to the value for the flag.
#' @param prefix flag prefix, usually "-" or "--".
#' @param sep separator to use if flag has a vector of values (default: NULL). 
#'
#' @return character vector of parsed commandline flags followed by their values
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' theFunction <- function(...){cmd_args_all()}
#' theArgs <- theFunction(arg1 = "value", arg2 = TRUE)
#' flagList <- cmd_list_interp(theArgs)
#' flags <- cmd_list_to_flags(flagList)
cmd_list_to_flags <- function(flagList, prefix = "-", sep = ","){
  
  if (is.null(flagList)) return(NULL)
  if (length(flagList) == 0) return(NULL)
  
  testthat::expect_named(flagList)
  
  flags <- purrr::imap(flagList, ~{c(paste0(prefix, .y), paste0(.x, collapse = sep))}) %>% 
    unlist() %>% 
    purrr::set_names(NULL)
  
  flags <- flags[flags != ""]
  
  return(flags)
}
