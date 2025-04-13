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

test_that("BetaDistribution functionality testing", {
  shape1s <- c(0.5, 1, 5, 25)
  shape2s <- c(0.5, 5, 1, 15)

  purrr::walk2(shape1s, shape2s, function(shape1, shape2) {
    beta_dist <- BetaDistribution$new(
      shape1 = shape1,
      shape2 = shape2
    )

    #pdf testing
    x <- c(0.1, 0.4, 0.55, 0.7, 0.9)
    expect_equal(
      beta_dist$pdf(x),
      dbeta(x, shape1 = shape1, shape2 = shape2)
    )

    #cdf testing
    expect_equal(
      beta_dist$cdf(x),
      pbeta(x, shape1 = shape1, shape2 = shape2)
    )

    #quantile testing
    expect_equal(
      beta_dist$quantile(x),
      qbeta(x, shape1 = shape1, shape2 = shape2)
    )

    #random testing
    expect_equal(
      beta_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rbeta(4, shape1 = shape1, shape2 = shape2))
    )
  })
})


### Gamma Distribution testing ------------------------------------------------

test_that("GammaDistribution testing initiation and param checks", {

  expect_error(
    GammaDistribution$new(shape = c(1, 2), rate = 1),
    "shape parameter should be of length 1"
  )
  expect_error(
    GammaDistribution$new(shape = 1, rate = c(1, 2)),
    "rate parameter should be of length 1"
  )

  expect_error(
    GammaDistribution$new(shape = "b", rate = 1),
    "shape parameter should be numeric"
  )
  expect_error(
    GammaDistribution$new(shape = 1, rate = "a"),
    "rate parameter should be numeric"
  )

  expect_error(
    GammaDistribution$new(shape = 0, rate = 1),
    "shape parameter must be greater than 0"
  )
  expect_error(
    GammaDistribution$new(shape = 1, rate = 0),
    "rate parameter must be greater than 0"
  )
})

test_that("GammaDistribution functionality testing", {
  shapes <- c(0.5, 1, 5, 25)
  rates <- c(0.5, 5, 1, 15)

  purrr::walk2(shapes, rates, function(shape, rate) {
    gamma_dist <- GammaDistribution$new(
      shape = shape,
      rate = rate
    )

    #pdf testing
    x <- c(0.1, 0.3, shape/rate, shape/rate + 0.75, shape/rate + 2)
    expect_equal(
      gamma_dist$pdf(x),
      dgamma(x, shape = shape, rate = rate)
    )

    #cdf testing
    expect_equal(
      gamma_dist$cdf(x),
      pgamma(x, shape = shape, rate = rate)
    )

    #quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      gamma_dist$quantile(q),
      qgamma(q, shape = shape, rate = rate)
    )

    #random testing
    expect_equal(
      gamma_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rgamma(4, shape = shape, rate = rate))
    )
  })
})


### Exponential Distribution testing ------------------------------------------

test_that("ExponentialDistribution testing initiation and param checks", {

  expect_error(
    ExponentialDistribution$new(rate = c(1, 2)),
    "rate parameter should be of length 1"
  )

  expect_error(
    ExponentialDistribution$new(rate = "b"),
    "rate parameter should be numeric"
  )

  expect_error(
    ExponentialDistribution$new(rate = 0),
    "rate parameter must be greater than 0"
  )
})

test_that("ExponentialDistribution functionality testing", {
  rates <- c(0.5, 5, 1, 15)

  purrr::walk(rates, function(rate) {
    exp_dist <- ExponentialDistribution$new(
      rate = rate
    )

    #pdf testing
    x <- c(0.5, 1, 2, 4, 10)
    expect_equal(
      exp_dist$pdf(x),
      dexp(x, rate = rate)
    )

    #cdf testing
    expect_equal(
      exp_dist$cdf(x),
      pexp(x, rate = rate)
    )

    #quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      exp_dist$quantile(q),
      qexp(q, rate = rate)
    )

    #random testing
    expect_equal(
      exp_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rexp(4, rate = rate))
    )
  })
})
