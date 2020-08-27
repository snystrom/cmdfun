#' Parses commandline help options to return vector of valid flag names
#'
#' When using cmdfun to write lazy shell wrappers, the user can easily mistype
#' a commandline flag since there is not text completion. Some programs behave
#' unexpectedly when flags are typed incorrectly, and for this reason return uninformative error messages.
#'
#' `cmd_help_parse_flags` tries to grab flags from --help documentation which
#' can be used for error checking. It will try to parse flags following "-" or
#' "--" while ignoring hyphenated words in help text. Although this should cover
#' most use-cases, it may be necessary to write a custom help-text parser for
#' nonstandard tools. Inspect this output **carefully** before proceeding. Most
#' often, characters are leftover at the **end** of parsed names, which will
#' require additional parsing.
#'
#' @seealso \code{\link{cmd_help_flags_similar}} \code{\link{cmd_help_flags_suggest}}
#'
#' @param help_lines `character` vector containing the output of "command
#'   --help", or similar output. Optional: pass either `stdout`, or `stderr` output from
#'   processx::run(), must set `processx = TRUE`.
#' @param split_newline `logical(1)` if set to TRUE will split string on "\\n" before
#'   parsing (useful when parsing output from `processx`). 
#'
#' @return character vector of flag names parsed from help text
#' @export
#'
#' @examples
#' if (.Platform$OS.type == "unix" & file.exists("/bin/tar")) {
#' # below are two examples parsing the --help method of GNU tar 
#'
#' # with processx
#' if (require(processx)) {
#' out <- processx::run("tar", "--help", error_on_status = FALSE)
#' fn_flags <- cmd_help_parse_flags(out$stdout, split_newline = TRUE)
#' }
#'
#' # with system2
#' lines <- system2("tar", "--help", stderr = TRUE)
#' fn_flags <- cmd_help_parse_flags(lines)
#' 
#' # NOTE: some of the "tar" flags contain the extra characters: "\\[", "\\)", and ";"
#' # ie "one-top-level\[" which should be "one-top-level"
#' # These can be additionally parsed using
#' gsub("[\\[;\\)]", "", fn_flags)
#' }
#'
cmd_help_parse_flags <- function(help_lines, split_newline = FALSE){
  stopifnot(is.logical(split_newline))
  
  if (split_newline){
    help_lines <- strsplit(help_lines, "\n")[[1]]
  }
  
  help_lines %>% 
    help_flags_all() %>% 
    help_flag_names

}

#' Get flag names from parsed lines
#'
#' @param lines parsed flag lines where 1st word on each line is the flag name
#'
#' @return character vector of flag names
#' @noRd
help_flag_names <- function(lines){
  strsplit(lines, " ") %>%
    purrr::map_chr(~{
      .x[[1]]
    }) %>% 
    unique
}

#' Get vector of help lines for short (-) and long (--) flag definitions
#'
#' @param lines unprocessed help lines (with newlines trimmed if needed)
#'
#' @return a vector where each entry is a line where the first word is the flag name
#' @noRd
help_flags_all <- function(lines){
  
  # Preprocess lines
  parsed_lines <- lines %>% 
    # drop leading whitespace
    gsub("^ +", "", .) %>% 
    # grab lines beginning with flag prefix
    grep("^-{1,2}[^-]", ., value = TRUE) %>% 
    # parse - and -- flag entries, and put at beginning of line
    # combine into single vector for further processing
    {
      c(help_flags_long(.), help_flags_short(.))
    } %>% 
    # remove flag prefix
    gsub("^-+", "", .) %>% 
    # remove leading whitespace
    # in case help file uses an unusual prefix
    # I've seen this for some windows CMD help pages.
    gsub("^ +", "", .) %>% 
    # Drop empty lines
    gsub("^$", "", .) %>% 
    # Drop text after =
    gsub("=.+", "", .) %>% 
    # Remove commas
    gsub(",", "", .)
  return(parsed_lines)
  
}

#' Return "short" (-) flag definition lines
#'
#' @param cleaned_lines vector of preprocessed help lines
#'
#' @return vector of lines where first word is a flag defined with -
#' @noRd
help_flags_short <- function(cleaned_lines){
  cleaned_lines %>% 
    # remove up to flag prefix (-)
    gsub("^-{2}.+ -", "-", .) %>% 
    # Keep any single - flags
    grep("^-{1}[^-]", ., value = TRUE) 
}

#' Return "long" (--) flag definition lines
#'
#' @param cleaned_lines 
#'
#' @return vector of lines where first word is a flag defined with --
#' @noRd
help_flags_long <- function(cleaned_lines){
  cleaned_lines %>% 
    # remove up to flag prefix (--)
    gsub(".+ --", "--", .) %>% 
    # Drop any single - flags
    grep("^-{1}[^-]", ., invert = TRUE, value = TRUE) 
}

#' Suggest alternative name by minimizing Levenshtein edit distance between valid and invalid arguments
#'
#' @param command_flag_names character vector of valid names (can be output of \code{\link{cmd_help_parse_flags}})
#' @param flags a vector names correspond to values to be checked against `command_flag_names`
#' @param .fun optional function to apply to `command_flag_names` and `flags`
#'   before checking their values. If using a function to rename flags after
#'   `cmd_list_interp`, use that same function here. Can be useful for parsing help
#'   lines into R-friendly variable names for user-convenience. Can be function
#'   or `rlang`-style formula definition (ie `.fun = ~{foo(.x)}` is the same as
#'   `.fun = function(x){foo(x)}`). Note: if command_flag_names need additional
#'   parsing after \code{\link{cmd_help_parse_flags}}, it is best to do that
#'   preprocessing before passing them to this function.
#' @param distance_cutoff Levenshtein edit distance beyond which to suggest
#'   ??? instead of most similar argument (default = 3). Setting this too
#'   liberally will result in nonsensical suggestions.
#'
#' @return named vector where names are names from `flags` and their values are the suggested best match from `command_flag_names`
#' @export
#' 
#' @importFrom utils adist
#' 
#' @examples
#' # with a flagsList, need to pass names()
#' flagsList <- list("output" = "somevalue", "missplld" = "anotherValue")
#' cmd_help_flags_similar(c("output", "misspelled"), names(flagsList))
#' 
#' command_flags <- c("long-flag-name")
#' flags <- c("long_flag_naee")
#' cmd_help_flags_similar(command_flags, flags, .fun = ~{gsub("-", "_", .x)})
#' 
#' # returns NULL if no errors
#' cmd_help_flags_similar(c("test"), "test")
cmd_help_flags_similar <- function(command_flag_names, flags, .fun = NULL, distance_cutoff = 3L){

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
  # drop_distance needs to be a value larger than the cutoff (as low as distance_cutoff + 1)
  drop_distance <- distance_cutoff + 1L
  
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
#' user_flags <- list("output", "inpt")
#' valid_flags <- c("output", "input")
#' suggestions <- cmd_help_flags_similar(valid_flags, user_flags)
#' \dontrun{
#' # Throws error
#' cmd_help_flags_suggest(suggestions)
#' }
cmd_help_flags_suggest <- function(suggest_names){
  if (is.null(suggest_names)){return(NULL)}
  
  quote_name <- function(name) paste0("\"", name, "\"")
  
  suggestString <- paste(quote_name(suggest_names), 
                         quote_name(names(suggest_names)), 
                         sep = " instead of: ", 
                         collapse = "\n")
  usethis::ui_stop(paste0("\nInvalid flags. Did you mean:\n", suggestString))
}

