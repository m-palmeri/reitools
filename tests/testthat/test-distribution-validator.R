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
