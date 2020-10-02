#' Keep items by name from list
#' 
#' A pipe-friendly wrapper around `list[(names(list) %in% names]`.
#' 
#' This function is slightly faster than using [cmd_list_keep()] to keep items
#' by name.
#' 
#' @param list an R list
#' @param names vector of names to keep
#'
#' @return list keeping only items defined by names
#' @export
#' 
#' @examples
#' cmd_list_keep_named(list("a" = 1, "b" = 2), "a")
cmd_list_keep_named <- function(list, names){
  list[(names(list) %in% names)]
}

#' Drop items by name from list
#'
#' A pipe-friendly wrapper around `list[!(names(list) %in% names)]`

#' This function is slightly faster than using [cmd_list_drop()] to drop items
#' by name.
#' 
#' @param list an R list
#' @param names vector of names to drop
#'
#' @return list removing items defined by names
#' @export
#'
#' @examples
#' cmd_list_drop_named(list("a" = 1, "b" = 2), "a")
cmd_list_drop_named <- function(list, names){
  list[!(names(list) %in% names)]
}

#' keep entries from list of flags by name, name/value pair, or index
#'
#' @param flags named list output of cmd_list_interp
#' @param keep vector of flag entries to keep. Pass a character vector
#'   to keep flags by name. Pass a named vector to keep flags by name/value
#'   pairs. Pass a numeric vector to keep by position.
#'
#' @return flags list with values not in keep removed
#' @export
#'
#' @examples
#' exFlags <- list("flag1" = 2, "flag2" = "someText")
#' cmd_list_keep(exFlags, "flag1")
#' # will keep flag2 because its name and value match 'keep' vector
#' cmd_list_keep(exFlags, c("flag2" = "someText"))
#' # Will keep "flag1" by position index
#' cmd_list_keep(exFlags, 1)
#'
#' # won't keep flag2 because its value isn't 'someText'
#' exFlags2 <- list("flag1" = 2, "flag2" = "otherText")
#' cmd_list_keep(exFlags, c("flag2" = "someText"))
cmd_list_keep <- function(flags, keep){
  
  testthat::expect_named(flags)
  
  if (length(keep) == 0){
    return(list())
  }
  
  if (is.numeric(keep)){
    return(list_keep_index(flags, keep))
  }
  
  if (is.null(names(keep))){
    keeps <- list_index_names(flags, keep)
    return(cmd_list_keep(flags, keeps))
  }

  if (!is.null(names(keep))) {
    keeps <- list_index_named_values(flags, keep)
    return(list_keep_index(flags, keeps))
  }
  
}

#' Drop entries from list of flags by name, name/value pair, or index
#'
#' @param flags named list output of cmd_list_interp
#' @param drop vector of flag entries to drop. Pass a character vector
#'   to drop flags by name. Pass a named vector to drop flags by name/value
#'   pairs. Pass a numeric vector to drop by position.
#'
#' @return flags list with values in drop removed
#' @export
#'
#' @examples
#' exFlags <- list("flag1" = 2, "flag2" = "someText")
#' cmd_list_drop(exFlags, "flag1")
#' # will drop flag2 because its name and value match 'drop' vector
#' cmd_list_drop(exFlags, c("flag2" = "someText"))
#' # Will drop "flag1" by position index
#' cmd_list_drop(exFlags, 1)
#'
#' # won't drop flag2 because its value isn't 'someText'
#' exFlags2 <- list("flag1" = 2, "flag2" = "otherText")
#' cmd_list_drop(exFlags, c("flag2" = "someText"))
cmd_list_drop <- function(flags, drop){
  
  testthat::expect_named(flags)
  
  if (length(drop) == 0){
    return(flags)
  }
  
  if (is.numeric(drop)){
    return(list_drop_index(flags, drop))
  }
  
  if (is.null(names(drop))){
    drops <- list_index_names(flags, drop)
    return(cmd_list_drop(flags, drops))
  }

  if (!is.null(names(drop))) {
    drops <- list_index_named_values(flags, drop)
    return(list_drop_index(flags, drops))
  }
  
}

#' keeps list entry by positional index
#'
#' @param flags list of flags
#' @param index position in list to keep
#'
#' @return flags kept at indices
#'
#' @examples
#' 
#' @noRd
list_keep_index <- function(flags, index){
  
  if (length(index) == 0 | all(is.na(index))){ return(list()) }
  
  index <- index[!is.na(index)]
  
  flags[index]
}

#' drops list entry by positional index
#'
#' @param list a `list`
#' @param index position in list to drop
#'
#' @return list w/ values dropped at indices
#'
#' @examples
#' 
#' @noRd
list_drop_index <- function(list, index){
  
  if (length(index) == 0 | all(is.na(index))){ return(list) }
  
  index <- index[!is.na(index)]
  
  list[-index]
}

#' Return index of named list values
#'
#' @param list a named list
#' @param names names of list entries
#'
#' @return vector of indices corresponding to named values matching names
#'
#' @examples
#' 
#' @noRd
list_index_names <- function(list, names){
  i <- which(names(list) %in% names)
  names(i) <- NULL
  
  if (length(i) == 0) { return(NULL) }
  
  return(i)
}

#' Return index for list entry matching name & value
#' 
#' 
#'
#' @param list a named list
#' @param named_values named character vector of list objects + their values to be matched.
#'
#' @return index in flags list with entries matching name/value pairs in named_values
#'
#' @examples
#' myList <- list(a = 1, b = 2, b = 5)
#' list_index_named_values(myList, c("b" = 5))
#' list_index_named_values(myList, c("b" = 2, "a" = 1))
#' 
#' @noRd
list_index_named_values <- function(list, named_values){
  
  testthat::expect_named(named_values)
  
  if (any(names(named_values) == "")){
    warnVals <- named_values[names(named_values) == ""]
    warning(paste0("The following values have no names associated with them and will be ignored: ", warnVals))
  }
  
  if (any(is.na(named_values))){
    warnVals <- named_values[is.na(named_values)]
    warnNames <- names(warnVals)
    warning(paste0("The following names in named_values have NA values and will be ignored: ", warnNames))
  }
  
  indices <- purrr::imap_int(named_values, ~{
    i <- which(names(list) == .y & list == .x)
    names(i) <- NULL
    
    # NA will mean "no match"
    if (length(i) == 0){return(NA)}
    # ignore names of "" to avoid unexpected behavior
    if (.y == ""){return(NA)}
    
    return(i)
  }) %>% 
    purrr::set_names(NULL)
  return(indices[!is.na(indices)])
}


#' Check that file(s) exist, error if not
#'
#' @param files list or vector of paths to check
#'
#' @return nothing or error message for each missing file
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' cmd_error_if_missing(tempdir())
#' \dontrun{
#' # Throws error if file doesn't exist
#' cmd_error_if_missing(file.path(tempdir(), "notreal"))
#' }
cmd_error_if_missing <- function(files){
  
  if (length(files) > 1) {
    files %>%
      purrr::map(purrr::discard, file.exists) %>%
      purrr::compact() %>% 
      error_text_file_not_exist %>% 
      error_file_not_exist()
  }
  
  if (length(files) == 1) {
    if (!file.exists(files)) {
      files %>% 
        error_text_file_not_exist() %>% 
        error_file_not_exist()
      } 
    else {return(invisible())}
  } 
  
}

#' Generate error text that file doesn't exist
#' 
#' This is such a stupid way to do things.
#'
#' @param file path to file or files (as vector)
#'
#' @return file doesn't exist error text
#'
#' @examples
#' 
#' @noRd
error_text_file_not_exist <- function(file){
  # Ugh. I hate this. This whole error checking/validation system needs yet
  # another overhaul.
  if (is.null(file)){
    return(invisible())
  }
  if (length(file) == 0){
    return(invisible())
  }
  paste0(file, " was not found.", collapse = "\n")
}

#' Throw error that file doesn't exist
#' 
#' This system needs fixing.
#'
#' @param file path to file
#'
#' @return file doesn't exist error
#' @noRd
#'
#' @examples
#' file %>%
#'  error_text_file_not_exist() %>%
#'  error_file_not_exist() 
error_file_not_exist <- function(text){
  # No real error checking.
  if (is.null(text)){
    return(invisible())
  }
  stop(text)
}

#' Generates list of expected output files
#'
#' See documentation of cmd_file_expect() for more details about how this works
#'
#' @param ext file extension (no ".", ie "txt", "html")
#' @param prefix file name to be given each ext. If a character vector, must be equal length of ext or shorter
#' @param outdir optional directory where files should exist
#'
#' @return list of file paths by each ext or prefix (whichever is longer)
#' @export
#'
#' @examples
#' # Makes list for many file types of same prefix
#' # ie myFile.txt, myFile.html, myFile.xml
#' cmd_file_combn("myFile", c("txt", "html", "xml"))
#' 
#' # Makes list for many files of same type
#' # ie myFile1.txt, myFile2.txt, myFile3.txt
#' cmd_file_combn(c("myFile1", "myFile2", "myFile3"), "txt")
#'
cmd_file_combn <- function(prefix, ext, outdir = "."){
  # strip leading . from ext (ie allow .txt.gz or txt.gz)
  ext %<>% gsub("^\\.", "", .)
  
  if (length(prefix) > 1 & length(ext) > 1){
    
    file_combn <- combn_prefix_suffix(prefix, ext) 
    file_list <- file_combn %>% 
      paste(outdir, ., sep = "/") %>% 
      sanitize_path() %>% 
      purrr::set_names(file_combn) %>% 
      as.list()
    
    return(file_list)
  }
  
  files <- purrr::map2(ext, prefix, ~{
    file.path(outdir, paste0(.y, ".", .x)) %>% 
      sanitize_path()
  })

  if (length(ext) < length(prefix)){
    files %>%
      purrr::set_names(prefix) %>%
      return()
  } else {
    files %>%
      purrr::set_names(ext) %>%
      return()
  }

}

#' Create all pairwise combinations of two vectors, excluding self-pairs
#'
#' @param prefix 
#' @param suffix 
#'
#' @return vector of all combinations of prefix + suffix
#' @noRd
#'
#' @examples
#' combn_prefix_suffix(c("one", "two", "three"), c(1,2))
#' # Compare vs output of:
#' combine_and_merge(c(c("one", "two"), c(1,2))
combn_prefix_suffix <- function(prefix, suffix){
  
  prefix %<>% as.character
  suffix %<>% as.character
  
  # taking combn of c(prefix, suffix) will result in prefix+prefix and suffix+suffix
  # entries (ie self-combinations), so these need to be subtracted out to
  # produce the final set of prefix+suffix entries
  
  self_combn <- list(prefix, suffix) %>% 
    purrr::map(combine_and_merge) %>% 
    unlist()
  
  prefix_suffix_combn <- c(prefix, suffix) %>% 
    combine_and_merge()
  
  prefix_suffix_combn[!(prefix_suffix_combn %in% self_combn)]
}

#' Merge vector
#'
#' @param v vector of length 2
#' @param sep separator to join v[1] and v[2]
#'
#' @return 
#' @noRd
#'
#' @examples
#' merge_combn_vector(c("one", "two"))
merge_combn_vector <- function(v, sep = "."){
  stopifnot(length(v) == 2)
  paste(v[1], v[2], sep = sep)
}

#' Create all pairwise combinations of vector entries & join on separator
#'
#' @param vector a vector to create all pairwise combinations of. Pass vector of
#'   vectors for matrix-wise operation.
#' @param sep separator
#'
#' @return
#' @noRd
#'
#' @examples
#' combine_and_merge(1:3)
#' combine_and_merge(c(c("one", "two"), c(1,2))
combine_and_merge <- function(vector, sep = "."){
  utils::combn(vector, m = 2, simplify = FALSE) %>% 
    purrr::map_chr(merge_combn_vector, sep = sep)
}

#' Creates list of paths by file extension & checks they exist
#'
#' Ext or prefix can be a vector or single character. The shorter value will be
#' propagated across all values of the other. See Examples for details.
#' 
#' If files are not found, throws an error
#'
#' @param prefix name of file prefix for each extension.
#' @param ext vector of file extensions
#' @param outdir directory the files will be inside
#'
#' @return vector of valid file paths
#' @export
#'
#' @importFrom magrittr %T>%
#'
#' @examples
#' \dontrun{
#' # Expects many file types of same prefix
#' # ie myFile.txt, myFile.html, myFile.xml
#' cmd_file_expect("myFile", c("txt", "html", "xml"))
#' 
#' # Expects many files of same type
#' # ie myFile1.txt, myFile2.txt, myFile3.txt
#' cmd_file_expect(c("myFile1", "myFile2", "myFile3"), "txt")
#' 
#' # Expects many files with each prefix and each extension
#' # ie myFile1.txt, myFile1.html, myFile2.txt, myFile2.html
#' cmd_file_expect(c("myFile1", "myFile2"), c("txt", "html"))
#' 
#' }
#'
#' 
cmd_file_expect <- function(prefix, ext, outdir = "."){
  cmd_file_combn(prefix, ext, outdir) %T>%
    cmd_error_if_missing()
}


#' Checks if file exists, returns pretty status message
#'
#' @param file path to file
#'
#' @return ui_done or ui_oops printed to terminal.
#' @export
#'
#' @examples
#' cmd_ui_file_exists("/path/to/file.txt")
cmd_ui_file_exists <- function(file){
 
  if (!class(file) == "character") {
    stop("file must be a character")
  } 
  
  if (length(file) > 1) {
    stop("file must be length 1")
  }
  
  if (file.exists(file)) {
    usethis::ui_done(file)
  } else {
    usethis::ui_oops(file)
  }
  return(invisible(NULL))
}


#' Wrapper function for checking an install
#' 
#' This function can be lightly wrapped by package builders to build a user-friendly install checking function.
#'
#' @param path_search `function` output of `cmd_path_search()`
#' @param path user-override path to check (identical to `path` argument of `cmd_path_search()` output)
#'
#' @return pretty printed message indicating whether files exits or not. Green check = Yes, red X = No.
#' @export
#'
#' @examples
#' \dontrun{
#' path_search <- cmd_path_search(default = "/bin", utils = "ls")
#' cmd_install_check(path_search)
#' }
cmd_install_check <- function(path_search, path = NULL){
  if (!is.function(path_search)) {
    stop("path_search must be a function")
  }
  message("checking main install")
  
  x <- try(path_search(path = path) %>% cmd_ui_file_exists(), silent = TRUE)
  
  if (class(x) == "try-error") {
    cmd_ui_file_exists(path)
    return(invisible(NULL))
    }
  
  util_catch <- try(path_search(path = path, util = TRUE), silent = TRUE) 
  has_utils <- class(util_catch) != "try-error"
  
  if (has_utils){
    message("checking util installs")
    path_search(path = path, util = TRUE) %>% 
      purrr::walk(cmd_ui_file_exists)
    
    return(invisible(NULL))
  }
}
