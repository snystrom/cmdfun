# TODO:
# - add macro function for constructing full call for passing to system2, etc.

#' return function dots as named list
#'
#' @param ... dots from function call
#'
#' @return named list of kwargs from ...
#' @export
#'
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  theFunction(example = "hello", boolFlag = TRUE, vectorFlag = c(1,2,3))
getDots <- function(...){
  dots <- list(...)
  return(dots)
}

#' convert list from getDots to vector of formatted shell flags
#'
#' @param dots output from getDots
#' @param flag_lookup optional named vector to convert names from dots to a
#'   simplified shell flag, useful for providing aliases to single-letter flags
#' @param prefix flag prefix, usually - or --
#' @param sep separator to use when passing vector for a single flag
#'
#' @return named vector of shell flags with their values
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  theFunction(example = "hello", boolFlag = TRUE, vectorFlag = c(1,2,3))
#' theArgs <-  dotsToArgs(theDots)
dotsToArgs <- function(dots, flag_lookup = NULL, prefix = "-", sep = ","){
  
  testthat::expect_type(dots, "list")
  
  if (length(dots) == 0) { return(NULL) }
  
  testthat::expect_named(dots)
  
  if (any(flag_lookup == "TRUE" | flag_lookup == "FALSE")) {
    warning("flag_lookup may contain boolean definitions, which could cause unexpected behavior")
  }
  
  if (!is.null(flag_lookup)) {
    dots <- convert_names(dots, flag_lookup)
  }
  
  if (is.null(flag_lookup)) { 
    flag_lookup <- names(dots)
    names(flag_lookup) <- names(dots)
  }
  
  # Check for illegal characters in dots, print warning
  check_args_contain_illegal_flags(dots)

  # collapse logicals, T = include, replace for empty string
  dots <- true_to_empty(dots)

  # only FALSE logicals remain, so they are dropped
  dots <- drop_list_logicals(dots)
  
  # Warn if arguments are defined multiple times
  purrr::imap_dbl(flag_lookup, count_matched_args, dots) %>% 
    purrr::set_names(concatenate_args(flag_lookup)) %>% 
    find_multimatched_args() %>% 
    {purrr::walk(., warn_multimatched_arg)}

  # concatenate to vector of flag calls
  args <- purrr::imap_chr(dots, ~{paste0(prefix, .y, " ", paste0(.x, collapse = sep))}) %>% 
    purrr::set_names(NULL)
  
  return(args)
}

