context("Parse Help Flags")

test_that("Suggestions work",{
  valid_flags <- c("out", "threshold", "version")
  flagList <- list("out" = 2, "thresold" = 10, "version" = "v1")
  expect_equal(cmd_help_flags_similar(valid_flags, names(flagList)), c("thresold" = "threshold"))
  
  expect_error(cmd_help_flags_similar(valid_flags, names(flagList)) %>% 
                 cmd_help_flags_suggest(), "Did you mean:", class = "error")
  
  expect_null(cmd_help_flags_similar(valid_flags, c()))
  expect_null(cmd_help_flags_similar(valid_flags, list()))
  
  flagList <- list("out" = 2, "threshold" = 10, "version" = "v1", "logn-name" = 2)
  expect_equal(cmd_help_flags_similar(c(valid_flags, "long-name"), names(flagList), ~{gsub("-", "_", .x)}),
               c("logn_name" = "long_name"))
  
  # big edit distance > 3 resolves to ???
  cmd_help_flags_similar(valid_flags, names(flagList))
  expect_equal(cmd_help_flags_similar(valid_flags, names(flagList)), c("logn-name" = "???"))
})

test_that("Parsing Help Works", {
  helplines <- c("   --version prints the version name",
                 "   -e prints the e-value",
                 "   --outdir the output directory",
                 "blah --anotherflag"
                 )
  expect_equal(cmd_help_parse_flags(helplines), c("version", "outdir", "e"))
  
  processxLines <- c("   --version prints the version name\n   -e prints the e-value\n   --outdir the output directory\n blah --anotherflag")
  expect_equal(cmd_help_parse_flags(processxLines, split_newline = TRUE), c("version", "outdir", "e"))
})

test_that("Parsing Help Works with long/short combo", {
  # -e , --evalue checks that it can grab both with short/long orientation
  # --outdir, -o checks it can grab with long/short orientation
  helplines <- c("   --version prints the version name",
                 "   -e, --evalue prints the e-value",
                 "   --outdir, -o the output directory",
                 "blah --anotherflag"
                 )
  expect_equal(cmd_help_parse_flags(helplines), c("version", "evalue", "outdir", "e", "o"))
  
  processxLines <- c("   --version prints the version name\n   -e, --evalue prints the e-value\n   --outdir, -o the output directory\n blah --anotherflag")
  expect_equal(cmd_help_parse_flags(processxLines, split_newline = TRUE), c("version", "evalue", "outdir", "e", "o"))
})

test_that("Suggestion Error behaves correctly",{
  # No error if no suggestions
  expect_null(cmd_help_flags_suggest(NULL))
  expect_error(cmd_help_flags_suggest(c("tst" = "test")), "Did you mean", class = "error")
  expect_error(cmd_help_flags_suggest(c("tst" = "test")), class = "usethis_error")
})

