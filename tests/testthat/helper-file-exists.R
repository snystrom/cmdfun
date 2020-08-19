exist_file <- tempfile()
file.create(exist_file)

bad_file <- tempfile()

exist_file_list <- purrr::map(1:3, ~{tempfile()}) %T>%
  purrr::map(file.create)

exist_file_vector <- unlist(exist_file_list)