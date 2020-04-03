#' Keep items by name from list
#'
#' @param list an R list
#' @param names vector of names to keep
#'
#' @return list keeping only items defined by names
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
#' @return list removing items defined by names
#'
#' @export
#' 
#' @examples
#' drop_list_by_name(list("a" = 1, "b" = 2), "a")
drop_list_by_name <- function(list, names){
  list[!(names(list) %in% names)]
}

#' Drop values from vector matching one or more regexes
#' 
#' Sometimes it is easier to preprocess arguments to commandline flags before
#' removing known invalid entries or special circumstances. This function allows
#' quick removal of values by scanning the flags vector with multiple regexes.
#'
#' @param flags character vector of flags (typically output of argsToFlags())
#' @param regex vector of regexes to scan flags. Will remove any flags matching the regex.
#'
#' @return flags without flags matching regexes
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
