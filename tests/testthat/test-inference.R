
test_that("estimateFPLD only accepts numeric input", {
  expect_error(estimateFPLD(TRUE))
  expect_error(estimateFPLD("a"))
  expect_error(estimateFPLD(c(2, 3, "b")))
})

test_that("method_of_quantiles requires equal length of x and p_vec", {
  expect_error(method_of_quantiles(2, p_vec = c(1, 2)))
  expect_error(method_of_quantiles(1:10, p_vec = c(1, 2)))
})

test_that("estimateFPLD gives error if input has length 0", {
  expect_error(estimateFPLD(numeric(0)))
})

test_that("fpld_mle works well", {
  par = runif(5)
  x = rFPLD(1000, par)
  par2 = mle_fpld(x)
  expect_equal(mean(log(dFPLD(x, par))), mean(log(dFPLD(x, par2))), tolerance = 1)
})

test_that("method_of_quantiles works well", {
  par = runif(5)
  x = rFPLD(1000, par)
  par2 = method_of_quantiles(x)
  p = seq(.1, .9, by = .1)
  expect_equal(qFPLD(p, par), qFPLD(p, par2), tolerance = 1)
})


