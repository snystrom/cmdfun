# TODO:
# - Switch purrr calls to base R?
# - write tests
# - add function for constructing full call for passing to system2
# - ERROR CHECK that arg & user-friendly name aren't both used
# 	- Warn user, default behavior to use raw flag over user-friendly name
# - handle BOOL flags correctly

getDots <- function(...){
  dots <- eval(substitute(list(...)))
  return(dots)
}

dotsToArgs <- function(dots, flag_lookup = NULL){
  if (!is.null(flag_lookup)) {
    names(dots)[names(dots) %in% names(flag_lookup)] <- flag_lookup[names(dots)[names(dots) %in% names(flag_lookup)]]
  }

  # collapse to named vector
  named_dots <- purrr::map_chr(dots, as.character)

  # concatenate to vector of flag calls
  args <- purrr::map2_chr(names(named_dots), nd, ~{paste0("-", .x, " ", .y)})

  return(args)
}


