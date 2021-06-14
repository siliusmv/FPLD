test_that("crps is computed correctly for the FPLD", {
  par = runif(5)
  y = rFPLD(10, par)
  crps1 = crps_fpld(y, par)
  crps2 = NULL
  ρ = function(u, p) p * u - ifelse(u < 0, u, 0)
  for (i in seq_along(y)) {
    crps2[i] = integrate(function(p) 2 * ρ(y[i] - qFPLD(p, par), p), 0, 1)$value
  }
  expect_equal(crps1, crps2, tolerance = 1e-3)
})

test_that("simplified integrate_fpld is correct", {
  v1 = NULL; v2 = NULL
  for (i in 1:20) {
    par = runif(5)
    lower = runif(1)
    v1[i] = FPLD:::integrate_fpld(par, lower, 1)
    v2[i] = FPLD:::integrate_fpld_simple(par, lower)
  }
  expect_equal(v1, v2)
})

test_that("simplified integrate_p_times_fpld is correct", {
  v1 = NULL; v2 = NULL
  for (i in 1:20) {
    par = runif(5)
    v1[i] = FPLD:::integrate_p_times_fpld(par, 0, 1)
    v2[i] = FPLD:::integrate_p_times_fpld_simple(par)
  }
  expect_equal(v1, v2)
})
