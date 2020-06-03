context("Parse Help Flags")

test_that("Suggestions work",{
  valid_flags <- c("out", "threshold", "version")
  flagList <- list("out" = 2, "thresold" = 10, "version" = "v1")
  expect_equal(cmd_suggest_flag_names(valid_flags, names(flagList)), c("thresold" = "threshold"))
  
  expect_error(cmd_suggest_flag_names(valid_flags, names(flagList)) %>% 
                 cmd_error_suggest_flag_names(), "Did you mean:", class = "error")
  
  flagList <- list("out" = 2, "threshold" = 10, "version" = "v1", "logn-name" = 2)
  expect_equal(cmd_suggest_flag_names(c(valid_flags, "long-name"), names(flagList), ~{gsub("-", "_", .x)}),
               c("logn_name" = "long_name"))
  
  # big edit distance > 3 resolves to ???
  cmd_suggest_flag_names(valid_flags, names(flagList))
  expect_equal(cmd_suggest_flag_names(valid_flags, names(flagList)), c("logn-name" = "???"))
})

test_that("Parsing Help Works", {
  helplines <- c("   --version prints the version name",
                 "   -e prints the e-value",
                 "   --outdir the output directory",
                 "blah --anotherflag"
                 )
  expect_equal(cmd_help_flag_names(helplines), c("version", "e", "outdir"))
  
  processxLines <- c("   --version prints the version name\n   -e prints the e-value\n   --outdir the output directory\n blah --anotherflag")
  expect_equal(cmd_help_flag_names(processxLines, processx = TRUE), c("version", "e", "outdir"))
})

test_that("Suggestion Error behaves correctly",{
  # No error if no suggestions
  expect_null(cmd_error_suggest_flag_names(NULL))
  expect_error(cmd_error_suggest_flag_names(c("tst" = "test")), "Did you mean", class = "error")
  expect_error(cmd_error_suggest_flag_names(c("tst" = "test")), class = "usethis_error")
})

