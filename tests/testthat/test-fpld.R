

test_that("transformFPLDParams gives correct median and iqr", {
  par = runif(5)
  x = rFPLD(20000, par)
  par2 = transformFPLDParams(par)
  expect_equal(median(x), par2[1], tolerance = 1e-2)
  expect_equal(unname(diff(quantile(x, c(.25, .75)))), log(exp(par2[2]) + 1), tolerance = 1e-1)
})


test_that("dFPLD integrates to pFPLD", {
  par = runif(5)
  lower_p = runif(1)
  upper_p = runif(1, lower_p, 1)
  lower = qFPLD(lower_p, par)
  upper = qFPLD(upper_p, par)
  integral = integrate(dFPLD, lower, upper, par = par)$value
  expect_equal(integral, pFPLD(upper, par) - pFPLD(lower, par), tolerance = 1e-5)
})

test_that("pFPLD is the inverse of qFPLD", {
  par = runif(5)
  p = runif(100)
  expect_equal(p, pFPLD(qFPLD(p, par), par))
})

test_that("densqFPLD integrates to qFPLD", {
  par = runif(5)
  lower = runif(1)
  upper = runif(1, lower, 1)
  integral = integrate(densqFPLD, lower, upper, par = par)$value
  expect_equal(integral, qFPLD(upper, par) - qFPLD(lower, par), tolerance = 1e-5)
})

test_that("backtransformFPLDParams works as it should", {
  par = matrix(runif(5 * 100), nrow = 5)
  p2 = matrix(nrow = 5, ncol = 100)
  for (i in 1:100) {
    p2 = backtransformFPLDParams(transformFPLDParams(par))
  }
  expect_equal(par, p2)
})
