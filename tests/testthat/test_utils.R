
test_that("Can detect illegal flags", {
  expect_false(flag_is_illegal("a"))
  expect_true(flag_is_illegal("&echo"))
  expect_true(flag_is_illegal("@echo"))
})


test_that("drop flags", {
  flags <- list("arg1" = "test", "arg2" = "dontdrop", "arg2" = "dropMe")
  
  expect_equal(cmd_list_drop(flags, c("")), flags)
  expect_equal(cmd_list_drop(flags, c("these", "dont", "exist")), flags)
  expect_equal(cmd_list_drop(flags, 3), flags[c(1,2)])
  expect_equal(cmd_list_drop(flags, c(2,3)), flags[1])
  
  expect_equal(cmd_list_drop(flags, c("arg2")), list(arg1 = "test"))
  expect_equal(cmd_list_drop(flags, c("arg2" = "dropMe")), list(arg1 = "test", arg2 = "dontdrop"))
  expect_equal(cmd_list_drop(flags, c("arg2" = "DROOE")), flags)
  expect_equal(cmd_list_drop(flags, c("arg2" = "DROOE", "arg2" = "dropMe")), 
                          list(arg1 = "test", arg2 = "dontdrop"))
  
  moreFlags <- list(arg1 = "test", arg2 = "arg2", "arg2" = "test")
  expect_warning(cmd_list_drop(moreFlags, c("arg1" = "test", "arg2")), "values have no names")
  expect_equal(
    suppressWarnings(cmd_list_drop(moreFlags, c("arg1" = "test", "arg2"))), 
    list(arg2 = "arg2", "arg2" = "test")
    )
})

test_that("keep flags", {
  flags <- list("arg1" = "test", "arg2" = "dontdrop", "arg2" = "dropMe")
  
  expect_equal(cmd_list_keep(flags, c("")), list())
  expect_equal(cmd_list_keep(flags, c("these", "dont", "exist")), list())
  expect_equal(cmd_list_keep(flags, 3), flags[3])
  expect_equal(cmd_list_keep(flags, c(2,3)), flags[c(2,3)])
  
  expect_equal(cmd_list_keep(flags, c("arg2")), list(arg2 = "dontdrop", arg2 = "dropMe"))
  expect_equal(cmd_list_keep(flags, c("arg2" = "dropMe")), list(arg2 = "dropMe"))
  expect_equal(cmd_list_keep(flags, c("arg2" = "DROOE")), list())
  expect_equal(cmd_list_keep(flags, c("arg2" = "DROOE", "arg2" = "dropMe")), 
                          list(arg2 = "dropMe"))
  
  moreFlags <- list(arg1 = "test", arg2 = "arg2", "arg2" = "test")
  expect_warning(cmd_list_keep(moreFlags, c("arg1" = "test", "arg2")), "values have no names")
  expect_equal(
    suppressWarnings(cmd_list_keep(moreFlags, c("arg1" = "test", "arg2"))), 
    list(arg1 = "test")
    )
})

# These may be tested implicitly in above new tests
test_that("internal list helpers work",{
  myList <- list("one" = 1,
                 "two" = 1,
                 "three" = 1,
                 "three" = 2)
  
  
  expect_null(list_index_names(myList, NULL))
  expect_equal(list_index_names(myList, c("one")), 1)
  expect_equal(list_index_names(myList, c("one", "three")), c(1,3, 4))
  
  expect_equal(cmd_list_keep_named(myList, "one"), myList[1])
  expect_equal(cmd_list_drop_named(myList, "one"), myList[c(2,3,4)])
  
  expect_equal(list_index_named_values(myList, c("three" = 1)), 3)
  expect_equal(list_index_named_values(myList, c("one" = 1, "three" = 1)), c(1,3))
  
  context("ignores if NA values in named_values")
  expect_warning(list_index_named_values(myList, c("one" = NA, "three" = 1)), "have NA values and will be ignored")
  expect_equal(suppressWarnings(list_index_named_values(myList, c("one" = NA, "three" = 1))), 3)
  
  context("ignores unnamed values in input list")
  expect_equal(list_index_named_values(list(1, "one" = 1), c("one" = 1)), 2)
})

test_that("file_not_exist error is invisible if NULL or empty", {
  expect_invisible(error_text_file_not_exist(NULL))
  expect_invisible(error_text_file_not_exist(c()))
})

