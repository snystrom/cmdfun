#' Return all named arguments and arguments passed as dots from parent function call
#'
#' @return named list of all arguments passed to parent
#' @export
#'
#' @examples
#' theFunction <- function(arg1, ...) { getAllArgs() }
#' theArgs <-  theFunction(arg1 = "test", example = "hello")
getAllArgs <- function(){
  # Modified from:
  # https://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-function-in-r-with-names
  # using callstack position of parent call will always evaluate to function this was called inside,
  # this allows this to work inside pipes
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     expand.dots = TRUE))[-1]
  
  # arguments from callstack need to be evaluated
  lapply(argList, eval)
  
}

#' return function dots from parent function as named list
#'
#'
#' @return named list of kwargs from ...
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' theFunction <- function(...) { getDotArgs() }
#' theDots <-  theFunction(example = "hello", boolFlag = TRUE, vectorFlag = c(1,2,3))
getDotArgs <- function(){
  
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     expand.dots = FALSE))[-1]
  
  lapply(argList[["..."]], eval) 
}

#' Return all named arguments from parent function call
#'
#' @return named list of all defined function arguments from parent
#' @export
#'
#' @examples
#' theFunction <- function(arg1, ...) { getNamedArgs() }
#' theNamedArgs <-  theFunction(arg1 = "test", example = "hello")
getNamedArgs <- function(){
  # see getAllNamedArgs for explanation of how this chunk works
  argList <- as.list(match.call(definition = sys.function(sys.parent()),
                     call = sys.call(sys.parent()), 
                     expand.dots = FALSE))[-1]
  
  lapply(argList, eval) %>% 
    drop_list_by_name("...")
}


#' convert list from getDots to vector of formatted shell flags
#'
#' @param args output from get*Args() family of functions (a named list)
#' @param flag_lookup optional named vector to convert names from dots to a
#'   simplified shell flag, useful for providing aliases to single-letter flags
#' @param prefix flag prefix, usually - or --
#' @param sep separator to use when passing vector for a single flag
#'
#' @return named vector of shell flags with their values
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#'
#' @examples
#' theFunction <- function(...) { getDotArgs() }
#' theDots <-  theFunction(example = "hello", boolFlag = TRUE, vectorFlag = c(1,2,3))
#' theFlags <-  argsToFlags(theDots)
argsToFlags <- function(args, flag_lookup = NULL, prefix = "-", sep = ","){
  
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
    flag_lookup <- names(args)
    names(flag_lookup) <- names(args)
  }
  
  # Check for illegal characters in dots, print warning
  check_args_contain_illegal_flags(args)

  args %<>% 
    # collapse logicals, T = include, replace for empty string
    true_to_empty() %>% 
    # only FALSE logicals remain, so they are dropped
    drop_list_logicals() %>% 
    # NULL flags should be removed
    drop_list_NULL() %>% 
    # Remove anything with empty names (happens )
    drop_list_by_name("")
  
  # Warn if arguments are defined multiple times
  purrr::imap_dbl(flag_lookup, count_matched_args, args) %>% 
    purrr::set_names(concatenate_args(flag_lookup)) %>% 
    find_multimatched_args() %>% 
    {purrr::walk(.data, warn_multimatched_arg)}

  # concatenate to vector of flag calls
  flags <- purrr::imap_chr(args, ~{paste0(prefix, .y, " ", paste0(.x, collapse = sep))}) %>% 
    purrr::set_names(NULL)
  
  return(flags)
}

