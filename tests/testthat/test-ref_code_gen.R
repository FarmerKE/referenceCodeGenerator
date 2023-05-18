test_that("generate ref code", {
  expect_equal(startsWith(x <- reference_code("SH"), "SH"), startsWith(x, "SH"))
})
