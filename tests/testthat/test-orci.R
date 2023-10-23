test_that("this is indeed OR", {
  expect_equal(OR_95CI(0, 0.2, 0.95, 2), "1 (0.99,  1.01)")
})
