#library(cmdfun)
## TODO: format tests inside test_that()


# test empty list
expect_equal(cmd_list_interp(list()), NULL)
# test named but NULL
expect_equal(cmd_list_interp(list("test" = NULL)), NULL)
# test NA values
expect_equal(cmd_list_interp(list("arg" = NA)), NULL)

# Test list vs vec
expect_success(cmd_list_interp(list("a" = 1)))
expect_failure(cmd_list_interp(c("a" = 1)))

# Null is dropped like FALSE
expect_equal(cmd_list_interp(list("b" = NULL)), cmd_list_interp(list("b" = FALSE)))

# Test multiple inputs sep default is ","
expect_equal(cmd_list_interp(list("a" = c(1,2,3))) %>% cmd_list_to_flags(), c("-a", "1,2,3"))
# Test multiple inputs w/ NULL sep
expect_equal(cmd_list_interp(list("a" = c(1,2,3))) %>% cmd_list_to_flags(sep = NULL), c("-a", "1", "2", "3"))
# Test multiple inputs w/ comma sep
expect_equal(cmd_list_interp(list("a" = c(1,2,3))) %>% cmd_list_to_flags(sep = ","), c("-a", "1,2,3"))

# test quo vs unquoted names, all combinations
expect_success(cmd_list_interp(list("a" = 1, "b" = T, "c" = F,
                               d = 1, e = T, f = F)))

expect_equal(
  cmd_list_interp(list("a" = 2, "b"= T, "x" = "x", "c" = NULL, "d" = F)),
              list("a" = 2, "b" = "", "x" = "x")
)

# pass non-list to dotsToArgs
expect_equal(cmd_list_interp(list("a" = 2)), list("a" = 2))
expect_equal(cmd_list_interp(list("a" = 2, "b" = 3, "c" = 4)) %>% cmd_list_to_flags(), c("-a", "2", "-b", "3", "-c", "4"))

# Check False gets dropped correctly for list and vector
expect_equal(cmd_list_interp(list("a" = 2, "b" = T, "c" = F)), list("a" = 2, "b" = ""))
expect_equal(cmd_list_interp(list("a" = 2, "b" = T, "c" = F, "d" = c(1,3,"test"))), 
             list("a" = 2, "b" = "", "d" = c("1","3","test")))

expect_equal(cmd_list_interp(list("a" = 2, "b" = T, "c" = F)) %>% cmd_list_to_flags(), c("-a", "2", "-b"))
expect_equal(cmd_list_interp(list("a" = 2, "b" = T, "c" = F, "d" = c(1,3,"test"))) %>% cmd_list_to_flags(), 
             c("-a" ,"2", "-b", "-d", "1,3,test"))

# Input must be named, should be caught by internal checking
expect_error(cmd_list_interp(list(2,3)), class = "expectation_failure")
expect_error(cmd_list_interp(c(2,3)), class = "expectation_failure")

# Dictionary tests
argsDict <- c("long1" = "l",
              "long2" = "ll")
argsDict_bool <- c(argsDict,
                   test = T)


## List & vector (modify when conversion complete to warn, etc.)
expect_equal(cmd_list_interp(list("long1" = 2, "long2" = T), argsDict), 
             list("l"= 2, "ll" = ""))

## Should warn if dict contains BOOL values, invalid entry
expect_warning(cmd_list_interp(list("a" = 2), argsDict_bool))
expect_warning(cmd_list_interp(list("long1" = 2, "long2" = T, "test" = "abc"), argsDict_bool))

## should warn if flag has both T & F ?
#expect_warning(dotsToArgs(list("b" = T, "b" = F)))

## should warn if flag has both T & F (with dictionary)
#expect_warning(dotsToArgs(list("long1" = T, "long1" = F), argsDict))

## Should also catch if long & short version of flags are both set
expect_equal(cmd_list_interp(list("long1" = 2, "l" = T)), 
             list("long1" = 2, "l" = ""))
expect_message(cmd_list_interp(list("long1" = 2, "l" = T), argsDict))

expect_message(cmd_list_interp(list("l" = T, "l" = T), argsDict))
expect_message(cmd_list_interp(list("l" = T, "l" = T)))

test_that("Illegal flags detected", {
  expect_error(cmd_list_interp(list("&&echo" = "test")))
  expect_error(cmd_list_interp(list("$(echo hello world)" = T)))
  expect_error(cmd_list_interp(list("test flag" = T)))
  expect_error(cmd_list_interp(list("<(cat.)" = T)))
})


test_that("to_flags works", {
  expect_null(cmd_list_to_flags(list()))
  expect_error(cmd_list_to_flags())
})
