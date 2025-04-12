### Normal Distribution testing -----------------------------------------------

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

test_that("NormalDistribution functionality testing", {
  means <- c(0, 1, 100, 20)
  sds <- c(1, 0.2, 5, 1000)

  purrr::walk2(means, sds, function(mean, sd) {
    normal_dist <- NormalDistribution$new(
      mean = mean,
      sd = sd
    )

    #pdf testing
    x <- c(mean - sd, mean - sd/2, mean + sd/3, mean + 2*sd)
    expect_equal(
      normal_dist$pdf(x),
      dnorm(x, mean = mean, sd = sd)
    )

    #cdf testing
    expect_equal(
      normal_dist$cdf(x),
      pnorm(x, mean = mean, sd = sd)
    )

    #quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      normal_dist$quantile(q),
      qnorm(q, mean = mean, sd = sd)
    )

    #random testing
    expect_equal(
      normal_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rnorm(4, mean = mean, sd = sd))
    )
  })
})


### Beta Distribution testing -------------------------------------------------

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
