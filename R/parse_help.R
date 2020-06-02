#' Parses commandline help options to return vector of valid flag names
#'
#' When using cmdlr to write lazy shell wrappers, the user can easily mistype
#' a commandline flag since there is not text completion. Some programs behave
#' unexpectedly when flags are typed incorrectly, and for this reason return uninformative error messages.
#'
#' `cmd_help_flag_names` tries to grab flags from --help documentation which can be used for error checking.
#'
#' @seealso \code{\link{cmd_suggest_flag_names}} \code{\link{cmd_error_cmd_suggest_flag_names}}
#'
#' @param help_lines `character` vector contianing the output of "command
#'   --help", or similar output. Optional: pass either `stdout`, or `stderr` output from
#'   processx::run(), must set `processx = TRUE`.
#' @param processx `logical(1)` if set to TRUE will split string on "\\n" before
#'   parsing. 
#'
#' @return first word entry of each line prefixed by "-". typically this grabs
#'   all flags from --help output, but often times will require extra
#'   postprocessing to remove commas or equal signs, for example.
#' @export
#'
#' @examples
#' \dontrun{
#' # with processx
#' out <- processx::run("tar", "--help", error_on_status = F)
#' lines <- strsplit(out$stderr, "\n")[[1]]
#' fn_flags <- cmd_help_flag_names(lines)
#'
#' # with system2
#' lines <- system2("tar", "--help", stderr = T)
#' fn_flags <- cmd_help_flag_names(lines)
#' }
#'
cmd_help_flag_names <- function(help_lines, processx = FALSE){
  
  stopifnot(is.logical(processx))
  
  if (processx){
    help_lines <- strsplit(help_lines, "\n")[[1]]
  }
  
  # drop leading whitespace
  flag_lines <- gsub("^ +", "", help_lines) 
  # grab lines beginning with flag prefix
  flag_lines <- grep("^-{1,2}[^-]", flag_lines, value = T)
  # remove flag prefix
  flag_lines <- gsub("^-+", "", flag_lines)
  # remove leading whitespace
  # in case help file uses an unusual prefix
  # I've seen this for some windows CMD help pages.
  flag_lines <- gsub("^ +", "", flag_lines)
  # Drop empty lines
  flag_lines <- gsub("^$", "", flag_lines)

  flag_names <- strsplit(flag_lines, " ") %>%
    purrr::map_chr(~{
      .x[[1]]
    })
  return(flag_names)
}

#' Suggest alternative name by Levenshtein edit distance
#'
#' @param command_flag_names character vector of valid names (can be output of \code{\link{cmd_help_flag_names}})
#' @param flags a vector names correspond to values to be checked against `command_flag_names`
#' @param .fun optional function to apply to `command_flag_names` and `flags`
#'   before checking their values. If using a function to rename flags after
#'   `cmd_args_to_flags`, use that same function here. Can be useful for parsing help
#'   lines into R-friendly variable names for user-convenience. Can be function
#'   or `rlang`-style formula defintion (ie `.fun = ~{foo(.x)}` is the same as
#'   `.fun = function(x){foo(x)}`). Note: if command_flag_names need additional
#'   parsing after \code{\link{cmd_help_flag_names}}, it is best to do that
#'   preprocessing before passing them to this function.
#'
#' @return named vector where names are names from `flags` and their values are the suggested best match from `command_flag_names`
#' @export
#' 
#' @importFrom utils adist
#' 
#' @examples
#' # with a flagsList, need to pass names()
#' flagsList <- list("output" = "somevalue", "missplld" = "anotherValue")
#' cmd_suggest_flag_names(c("output", "misspelled"), names(flagsList))
#' 
#' command_flags <- c("long-flag-name")
#' flags <- c("long_flag_naee")
#' cmd_suggest_flag_names(command_flags, flags, .fun = ~{gsub("-", "_", .x)})
#' 
#' # returns NULL if no errors
#' cmd_suggest_flag_names(c("test"), "test")
cmd_suggest_flag_names <- function(command_flag_names, flags, .fun = NULL){

  if (!is.null(.fun)){
    if (class(.fun) == "formula"){.fun <- rlang::as_function(.fun)}
    stopifnot(is.function(.fun))
    
    command_flag_names <- .fun(command_flag_names)
    flags <- .fun(flags)
  }
  
  bad_flags <- flags[!flags %in% command_flag_names]
  
  if (length(bad_flags) == 0) {return(NULL)}

  flag_dist <- adist(bad_flags, command_flag_names)
  
  
  # Only suggest names similar enough to existing flag,
  # otherwise return ??? for match.
  # distance_cutoff is the levenshtein edit distance threshold
  # drop_distance is a special value for things to be dropped. Because I minimize edit distance,
  # drop_distance needs to be a large value (could theoretically be as low as distance_cutoff + 1),
  # but setting it to 99L for now.
  drop_distance <- 99L
  distance_cutoff <- 3
  
  flag_dist[flag_dist > distance_cutoff] <- drop_distance
  i <- apply(flag_dist, 1, function(x) {which(x == min(x))[1]})
  drop <- apply(flag_dist, 1, function(x) {which(min(x) == drop_distance)[1]})
  suggest_flags <- command_flag_names[i]
  names(suggest_flags) <- bad_flags
  suggest_flags[!is.na(drop)] <- "???"
  return(suggest_flags)
}

#' Error & Suggest different flag name to user
#'
#' @param suggest_names named character vector, names correspond to original
#'   value, values correspond to suggested replacement.
#'
#' @return error message suggesting alternatives to user
#' @export
#'
#' @examples
#' flags <- list("output" = "somevalue", "missplld" = "anotherValue")
#' suggestions <- cmd_suggest_flag_names(c("output", "misspelled"), flags)
#' \dontrun{
#' cmd_error_cmd_suggest_flag_names(suggestions)
#' }
cmd_error_cmd_suggest_flag_names <- function(suggest_names){
  if (is.null(suggest_names)){return(NULL)}
  
  quote_name <- function(name) paste0("\"", name, "\"")
  
  suggestString <- paste(quote_name(suggest_names), 
                         quote_name(names(suggest_names)), 
                         sep = " instead of: ", 
                         collapse = "\n")
  usethis::ui_stop(paste0("\nInvalid flags. Did you mean:\n", suggestString))
}

