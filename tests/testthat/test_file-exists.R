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
  expect_invisible(cmd_error_if_missing(exist_file_list))
  expect_invisible(cmd_error_if_missing(exist_file_vector))
  expect_invisible(cmd_error_if_missing(exist_file))
  expect_error(cmd_error_if_missing(bad_file), "was not found")
})

test_that("UI file exists works", {
  expect_invisible(cmd_ui_file_exists(exist_file))
  expect_message(cmd_ui_file_exists(exist_file), cli::symbol$tick)
  expect_message(cmd_ui_file_exists(bad_file), cli::symbol$cross)
  expect_error(cmd_ui_file_exists(exist_file_vector), "length 1")
  expect_error(cmd_ui_file_exists(exist_file_list))
})
