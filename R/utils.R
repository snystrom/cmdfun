#' Keep items by name from list
#'
#' @param list an R list
#' @param names vector of names to keep
#'
#' @return
#' 
#' @export
#'
#' @examples
#' keep_list_by_name(list("a" = 1, "b" = 2), "a")
keep_list_by_name <- function(list, names){
  list[(names(list) %in% names)]
}

#' Drop items by name from list
#'
#' @param list an R list
#' @param names vector of names to drop
#'
#' @return
#'
#' @export
#' 
#' @examples
#' drop_list_by_name(list("a" = 1, "b" = 2), "a")
drop_list_by_name <- function(list, names){
  list[!(names(list) %in% names)]
}

#' Title
#'
#' @param flags
#' @param regex 
#'
#' @return
#' @export
#'
#' @examples
#' flags <- c("-n value", "-f")
#' drop_flags_regex(flags, "-n")
#' drop_flags_regex(flags, "-n value")
drop_flags_regex <- function(flags, regex){
  lapply(regex, function(re){
    flags <<- flags[!grepl(re, flags)]
  })
  return(flags)
}
