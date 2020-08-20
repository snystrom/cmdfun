dir <- tempdir()

exist_file_list <- cmd_file_cmbn("txt", c("a", "b", "c"), dir) %T>%
  purrr::walk(file.create)

bad_file <- tempfile()

exist_file_vector <- unlist(exist_file_list)
