library(dotargs)
## TODO: format tests inside test_that()

# test empty list
expect_equal(argsToFlags(list()), NULL)

# Test list vs vec
expect_success(argsToFlags(list("a" = 1)))
expect_failure(argsToFlags(c("a" = 1)))

# Null is dropped like FALSE
expect_equal(argsToFlags(list("b" = NULL)), argsToFlags(list("b" = FALSE)))

# Test multiple inputs
expect_equal(argsToFlags(list("a" = c(1,2,3))), "-a 1,2,3")

# test quo vs unquoted names, all combinations
expect_success(argsToFlags(list("a" = 1, "b" = T, "c" = F,
                               d = 1, e = T, f = F)))

# pass non-list to dotsToArgs
expect_equal(argsToFlags(list("a" = 2)), "-a 2")
expect_equal(argsToFlags(list("a" = 2, "b" = 3, "c" = 4)), c("-a 2", "-b 3", "-c 4"))

# Check False gets dropped correctly for list and vector
expect_equal(argsToFlags(list("a" = 2, "b" = T, "c" = F)), c("-a 2", "-b "))
expect_equal(argsToFlags(list("a" = 2, "b" = T, "c" = F, "d" = c(1,3,"test"))), c("-a 2", "-b ", "-d 1,3,test"))

# Input must be named, should be caught by internal checking
expect_error(argsToFlags(list(2,3)))
expect_error(argsToFlags(c(2,3)))

# Dictionary tests
argsDict <- c("long1" = "l",
              "long2" = "ll")
argsDict_bool <- c(argsDict,
                   test = T)


## List & vector (modify when conversion complete to warn, etc.)
expect_equal(argsToFlags(list("long1" = 2, "long2" = T), argsDict), c("-l 2", "-ll "))

## Should warn if dict contains BOOL values, invalid entry
expect_warning(argsToFlags(list("a" = 2), argsDict_bool))
expect_warning(argsToFlags(list("long1" = 2, "long2" = T, "test" = "abc"), argsDict_bool))

## should warn if flag has both T & F ?
#expect_warning(dotsToArgs(list("b" = T, "b" = F)))

## should warn if flag has both T & F (with dictionary)
#expect_warning(dotsToArgs(list("long1" = T, "long1" = F), argsDict))

## Should also catch if long & short version of flags are both set
expect_equal(argsToFlags(list("long1" = 2, "l" = T)), c("-long1 2", "-l "))
expect_message(argsToFlags(list("long1" = 2, "l" = T), argsDict))

expect_message(argsToFlags(list("l" = T, "l" = T), argsDict))
expect_message(argsToFlags(list("l" = T, "l" = T)))

test_that("Illegal flags detected", {
  expect_error(argsToFlags(list("&&echo" = "test")))
  expect_error(argsToFlags(list("$(echo hello world)" = T)))
  expect_error(argsToFlags(list("test flag" = T)))
  expect_error(argsToFlags(list("<(cat.)" = T)))
})

