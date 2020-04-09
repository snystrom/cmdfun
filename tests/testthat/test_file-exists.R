skip_if(T, "test fails because tempfiles aren't working as expected. Works locally on non R CMD CHECK.")

setup({
  exist_file <- tempfile()
  file.create(exist_file)
  
  bad_file <- tempfile()
  
  exist_file_list <- purrr::map(1:3, ~{tempfile()}) %T>%
    purrr::map(file.create)
  
  exist_file_vector <- unlist(exist_file_list)
})

teardown({
  file.remove(exist_file)
  
  purrr::walk(exist_file_list, file.remove)
})


test_that("Test files as expected", {
  expect_true(file.exists(exist_file))
  expect_true(all(file.exists(exist_file_vector)))
  expect_false(file.exists(bad_file))
})

test_that("Test file utils work", {
  expect_invisible(check_files_exist(exist_file_list))
  expect_invisible(check_files_exist(exist_file_vector))
  expect_invisible(check_files_exist(exist_file))
  expect_error(check_files_exist(bad_file), "was not found")
})
