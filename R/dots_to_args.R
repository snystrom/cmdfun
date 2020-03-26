# TODO:
# - Switch purrr calls to base R?
# - write tests
# - add function for constructing full call for passing to system2
# - ERROR CHECK that arg & user-friendly name aren't both used
#   - Warn user, default behavior to use raw flag over user-friendly name
# - handle BOOL flags correctly (UNTESTED)
# - handle vector arguments

#' return function dots as named list
#'
#' @param ... dots from function call
#'
#' @return named list of kwargs from ...
#' @export
#'
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  theFunction(example = "hello", example2 = "world", boolFlag = TRUE, vectorFlag = c(1,2,3))
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
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  theFunction(example = "hello", example2 = "world", boolFlag = TRUE, vectorFlag = c(1,2,3))
#' theArgs <-  dotsToArgs(theDots)
dotsToArgs <- function(dots, flag_lookup = NULL, prefix = "-", sep = ","){
  
  testthat::expect_type(dots, "list")
  if (length(dots) == 0) { return("") }
  
  testthat::expect_named(dots)

  if (!is.null(flag_lookup)) {
    names(dots)[names(dots) %in% names(flag_lookup)] <- flag_lookup[names(dots)[names(dots) %in% names(flag_lookup)]]
  }

  # collapse logicals, T = include, F = exclude
  dots <- purrr::map(dots, ~{
    if (!is.logical(.x)) return(.x)

    if (.x == T) return("")

    return(.x)
  })

  # only FALSE logicals remain, so they are dropped
  dots <- dots[!purrr::map_lgl(dots, is.logical)]

  # concatenate to vector of flag calls
  args <- purrr::imap_chr(dots, ~{paste0(prefix, .y, " ", paste0(.x, collapse = sep))})
  args <- purrr::set_names(args, NULL)
  
  return(args)
}


