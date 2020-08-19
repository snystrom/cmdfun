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
  expect_invisible(cmd_files_exist(exist_file_list))
  expect_invisible(cmd_files_exist(exist_file_vector))
  expect_invisible(cmd_files_exist(exist_file))
  expect_error(cmd_files_exist(bad_file), "was not found")
})

