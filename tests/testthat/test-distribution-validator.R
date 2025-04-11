test_that("normal distribution validator testing", {
  validator <- NormalDistributionValidator$new()

  expect_error(
    validator$validate(list("a" = 1)),
    "params should include mean, sd"
  )

  expect_error(
    validator$validate(list("sd" = 1, "mean" = 1)),
    "params should have the order mean, sd"
  )

  expect_error(
    validator$validate(list("mean" = 1, "sd" = c(1, 2))),
    "each element of params should be of length 1"
  )

  expect_error(
    validator$validate(list("mean" = 1, "sd" = "a")),
    "each element of params should be numeric"
  )

  expect_error(
    validator$validate(list("mean" = 1, "sd" = 0)),
    "sd parameter must be greater than 0"
  )
})


test_that("beta distribution validator testing", {
  validator <- BetaDistributionValidator$new()

  expect_error(
    validator$validate(list("a" = 1)),
    "params should include alpha, beta"
  )

  expect_error(
    validator$validate(list("beta" = 1, "alpha" = 1)),
    "params should have the order alpha, beta"
  )

  expect_error(
    validator$validate(list("alpha" = 1, "beta" = c(1, 2))),
    "each element of params should be of length 1"
  )

  expect_error(
    validator$validate(list("alpha" = 1, "beta" = "a")),
    "each element of params should be numeric"
  )

  expect_error(
    validator$validate(list("alpha" = 1, "beta" = -1)),
    "alpha and beta parameters must be greater than 0"
  )

  expect_error(
    validator$validate(list("alpha" = 0, "beta" = 1)),
    "alpha and beta parameters must be greater than 0"
  )
})


test_that("validate_distribution_params helper function testing", {
  expect_no_error(validate_distribution_params(list(mean = 1, sd = 1), "normal"))
  expect_no_error(validate_distribution_params(list(mean = 0, sd = 1000), "normal"))
  expect_no_error(validate_distribution_params(list(alpha = 1, beta = 1), "beta"))
  expect_no_error(validate_distribution_params(list(alpha = 0.1, beta = 0.5), "beta"))
})
