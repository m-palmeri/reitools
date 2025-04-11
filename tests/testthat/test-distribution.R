test_that("NormalDistribution testing initiation and param checks", {

  expect_error(
    NormalDistribution$new(mean = c(1, 2), sd = 1),
    "mean parameter should be of length 1"
  )
  expect_error(
    NormalDistribution$new(mean = 1, sd = c(1, 2)),
    "sd parameter should be of length 1"
  )

  expect_error(
    NormalDistribution$new(mean = "b", sd = 1),
    "mean parameter should be numeric"
  )
  expect_error(
    NormalDistribution$new(mean = 1, sd = "a"),
    "sd parameter should be numeric"
  )

  expect_error(
    NormalDistribution$new(mean = 1, sd = 0),
    "sd parameter must be greater than 0"
  )
})


test_that("BetaDistribution testing initiation and param checks", {

  expect_error(
    BetaDistribution$new(shape1 = c(1, 2), shape2 = 1),
    "shape1 parameter should be of length 1"
  )
  expect_error(
    BetaDistribution$new(shape1 = 1, shape2 = c(1, 2)),
    "shape2 parameter should be of length 1"
  )

  expect_error(
    BetaDistribution$new(shape1 = "b", shape2 = 1),
    "shape1 parameter should be numeric"
  )
  expect_error(
    BetaDistribution$new(shape1 = 1, shape2 = "a"),
    "shape2 parameter should be numeric"
  )

  expect_error(
    BetaDistribution$new(shape1 = 0, shape2 = 1),
    "shape1 parameter must be greater than 0"
  )
  expect_error(
    BetaDistribution$new(shape1 = 1, shape2 = 0),
    "shape2 parameter must be greater than 0"
  )
})
