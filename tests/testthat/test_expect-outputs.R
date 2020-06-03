test_that("Expect Outputs Generates correct names",{
  expect_equal(cmd_output_expect(c("txt", "ext", "xml"), "myFile"),
               list(txt = "./myFile.txt",
                    ext = "./myFile.ext",
                    xml = "./myFile.xml"))

  expect_equal(cmd_output_expect("txt", c("myFile", "myFile1", "myFile2")),
               list(myFile = "./myFile.txt",
                    myFile1 = "./myFile1.txt",
                    myFile2 = "./myFile2.txt"))
})

test_that("Outdir works",{
  expect_equal(
    cmd_output_expect("txt", "myFile", outdir = "test/test"),
    cmd_output_expect("txt", "myFile", outdir = "test/test/")
  )
  
  expect_equal(
    cmd_output_expect("txt", "myFile", outdir = "test/test"),
    list("txt" = "test/test/myFile.txt")
  )
})
