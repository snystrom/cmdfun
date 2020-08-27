dir <- tempdir()

exist_prefix <- c("a", "b", "c")
exist_ext <- c("txt")
exist_file_combn_list <- cmd_file_combn(exist_prefix, exist_ext, dir) %T>%
  purrr::walk(file.create)

bad_file <- tempfile()

exist_file_vector <- unlist(exist_file_combn_list)
