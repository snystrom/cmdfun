library(dotargs)
## TODO: format tests inside test_that()

# test empty list
expect_equal(dotsToArgs(list()), NULL)

# Test list vs vec
expect_success(dotsToArgs(list("a" = 1)))
expect_failure(dotsToArgs(c("a" = 1)))

# Null is dropped like FALSE
expect_equal(dotsToArgs(list("b" = NULL)), dotsToArgs(list("b" = FALSE)))

# Test multiple inputs
expect_equal(dotsToArgs(list("a" = c(1,2,3))), "-a 1,2,3")

# test quo vs unquoted names, all combinations
expect_success(dotsToArgs(list("a" = 1, "b" = T, "c" = F,
                               d = 1, e = T, f = F)))

# pass non-list to dotsToArgs
expect_equal(dotsToArgs(list("a" = 2)), "-a 2")
expect_equal(dotsToArgs(list("a" = 2, "b" = 3, "c" = 4)), c("-a 2", "-b 3", "-c 4"))

# Check False gets dropped correctly for list and vector
expect_equal(dotsToArgs(list("a" = 2, "b" = T, "c" = F)), c("-a 2", "-b "))
expect_equal(dotsToArgs(list("a" = 2, "b" = T, "c" = F, "d" = c(1,3,"test"))), c("-a 2", "-b ", "-d 1,3,test"))

# Input must be named, should be caught by internal checking
expect_error(dotsToArgs(list(2,3)))
expect_error(dotsToArgs(c(2,3)))

# Dictionary tests
argsDict <- c("long1" = "l",
              "long2" = "ll")
argsDict_bool <- c(argsDict,
                   test = T)


## List & vector (modify when conversion complete to warn, etc.)
expect_equal(dotsToArgs(list("long1" = 2, "long2" = T), argsDict), c("-l 2", "-ll "))

## Should warn if dict contains BOOL values, invalid entry
expect_warning(dotsToArgs(list("a" = 2), argsDict_bool))
expect_warning(dotsToArgs(list("long1" = 2, "long2" = T, "test" = "abc"), argsDict_bool))

## should warn if flag has both T & F ?
#expect_warning(dotsToArgs(list("b" = T, "b" = F)))

## should warn if flag has both T & F (with dictionary)
#expect_warning(dotsToArgs(list("long1" = T, "long1" = F), argsDict))

## Should also catch if long & short version of flags are both set
expect_equal(dotsToArgs(list("long1" = 2, "l" = T)), c("-long1 2", "-l "))
expect_message(dotsToArgs(list("long1" = 2, "l" = T), argsDict))

expect_message(dotsToArgs(list("l" = T, "l" = T), argsDict))
expect_message(dotsToArgs(list("l" = T, "l" = T)))

# getDots checks
expect_type(getDots(a = 2, b = "myString", c = T, d = F), "list")

test_that("Illegal flags detected", {
  expect_error(dotsToArgs(list("&&echo" = "test")))
  expect_error(dotsToArgs(list("$(echo hello world)" = T)))
  expect_error(dotsToArgs(list("test flag" = T)))
  expect_error(dotsToArgs(list("<(cat.)" = T)))
})

