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
})


test_that("beta distribution validator testing", {
  validator <- BetaDistributionValidator$new()

  expect_error(
    validator$validate(list("a" = 1)),
    "params should include shape1, shape2"
  )

  expect_error(
    validator$validate(list("shape2" = 1, "shape1" = 1)),
    "params should have the order shape1, shape2"
  )

  expect_error(
    validator$validate(list("shape1" = 1, "shape2" = c(1, 2))),
    "each element of params should be of length 1"
  )

  expect_error(
    validator$validate(list("shape1" = 1, "shape2" = "a")),
    "each element of params should be numeric"
  )
})


test_that("validate_distribution_params helper function testing", {
  expect_no_error(validate_distribution_params(list(mean = 1, sd = 1), "normal"))
  expect_no_error(validate_distribution_params(list(mean = 0, sd = 1000), "normal"))
  expect_no_error(validate_distribution_params(list(shape1 = 1, shape2 = 1), "beta"))
  expect_no_error(validate_distribution_params(list(shape1 = 0.1, shape2 = 0.5), "beta"))
})
