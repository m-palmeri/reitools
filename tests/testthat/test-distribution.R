### Normal Distribution testing -----------------------------------------------

test_that("DistributionNormal testing initiation and param checks", {
  expect_error(
    DistributionNormal$new(mean = c(1, 2), sd = 1),
    "`mean` must be a number, not a double vector"
  )
  expect_error(
    DistributionNormal$new(mean = "b", sd = 1),
    "`mean` must be a number"
  )

  expect_error(
    DistributionNormal$new(mean = 1, sd = c(1, 2)),
    "`sd` must be a number, not a double vector"
  )
  expect_error(
    DistributionNormal$new(mean = 1, sd = "a"),
    "`sd` must be a number"
  )
  expect_error(
    DistributionNormal$new(mean = 1, sd = -1),
    "`sd` must be a number larger than or equal to 0"
  )
})

test_that("DistributionNormal functionality testing", {
  means <- c(0, 1, 100, 20)
  sds <- c(1, 0.2, 5, 1000)

  purrr::walk2(means, sds, function(mean, sd) {
    normal_dist <- DistributionNormal$new(
      mean = mean,
      sd = sd
    )

    # pdf testing
    x <- c(mean - sd, mean - sd / 2, mean + sd / 3, mean + 2 * sd)
    expect_equal(
      normal_dist$pdf(x),
      dnorm(x, mean = mean, sd = sd)
    )

    # cdf testing
    expect_equal(
      normal_dist$cdf(x),
      pnorm(x, mean = mean, sd = sd)
    )

    # quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      normal_dist$quantile(q),
      qnorm(q, mean = mean, sd = sd)
    )

    # random testing
    expect_equal(
      normal_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rnorm(4, mean = mean, sd = sd))
    )
  })
})


### Beta Distribution testing -------------------------------------------------

test_that("DistributionBeta testing initiation and param checks", {
  expect_error(
    DistributionBeta$new(shape1 = c(1, 2), shape2 = 1),
    "`shape1` must be a number"
  )
  expect_error(
    DistributionBeta$new(shape1 = "b", shape2 = 1),
    "`shape1` must be a number"
  )
  expect_error(
    DistributionBeta$new(shape1 = -1, shape2 = 1),
    "`shape1` must be a number larger than or equal to 0"
  )

  expect_error(
    DistributionBeta$new(shape1 = 1, shape2 = c(1, 2)),
    "`shape2` must be a number"
  )
  expect_error(
    DistributionBeta$new(shape1 = 1, shape2 = "a"),
    "`shape2` must be a number"
  )
  expect_error(
    DistributionBeta$new(shape1 = 1, shape2 = -0.5),
    "`shape2` must be a number larger than or equal to 0"
  )
})

test_that("DistributionBeta functionality testing", {
  shape1s <- c(0.5, 1, 5, 25)
  shape2s <- c(0.5, 5, 1, 15)

  purrr::walk2(shape1s, shape2s, function(shape1, shape2) {
    beta_dist <- DistributionBeta$new(
      shape1 = shape1,
      shape2 = shape2
    )

    # pdf testing
    x <- c(0.1, 0.4, 0.55, 0.7, 0.9)
    expect_equal(
      beta_dist$pdf(x),
      dbeta(x, shape1 = shape1, shape2 = shape2)
    )

    # cdf testing
    expect_equal(
      beta_dist$cdf(x),
      pbeta(x, shape1 = shape1, shape2 = shape2)
    )

    # quantile testing
    expect_equal(
      beta_dist$quantile(x),
      qbeta(x, shape1 = shape1, shape2 = shape2)
    )

    # random testing
    expect_equal(
      beta_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rbeta(4, shape1 = shape1, shape2 = shape2))
    )
  })
})


### Gamma Distribution testing ------------------------------------------------

test_that("DistributionGamma testing initiation and param checks", {
  expect_error(
    DistributionGamma$new(shape = c(1, 2), rate = 1),
    "`shape` must be a number"
  )
  expect_error(
    DistributionGamma$new(shape = "b", rate = 1),
    "`shape` must be a number"
  )
  expect_error(
    DistributionGamma$new(shape = -1, rate = 1),
    "`shape` must be a number larger than or equal to 0"
  )

  expect_error(
    DistributionGamma$new(shape = 1, rate = c(1, 2)),
    "`rate` must be a number"
  )
  expect_error(
    DistributionGamma$new(shape = 1, rate = "a"),
    "`rate` must be a number"
  )
  expect_error(
    DistributionGamma$new(shape = 1, rate = -1),
    "`rate` must be a number larger than or equal to 0"
  )
})

test_that("DistributionGamma functionality testing", {
  shapes <- c(0.5, 1, 5, 25)
  rates <- c(0.5, 5, 1, 15)

  purrr::walk2(shapes, rates, function(shape, rate) {
    gamma_dist <- DistributionGamma$new(
      shape = shape,
      rate = rate
    )

    # pdf testing
    x <- c(0.1, 0.3, shape / rate, shape / rate + 0.75, shape / rate + 2)
    expect_equal(
      gamma_dist$pdf(x),
      dgamma(x, shape = shape, rate = rate)
    )

    # cdf testing
    expect_equal(
      gamma_dist$cdf(x),
      pgamma(x, shape = shape, rate = rate)
    )

    # quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      gamma_dist$quantile(q),
      qgamma(q, shape = shape, rate = rate)
    )

    # random testing
    expect_equal(
      gamma_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rgamma(4, shape = shape, rate = rate))
    )
  })
})


### Exponential Distribution testing ------------------------------------------

test_that("DistributionExponential testing initiation and param checks", {
  expect_error(
    DistributionExponential$new(rate = c(1, 2)),
    "`rate` must be a number"
  )

  expect_error(
    DistributionExponential$new(rate = "b"),
    "`rate` must be a number"
  )

  expect_error(
    DistributionExponential$new(rate = -1),
    "`rate` must be a number larger than or equal to 0"
  )
})

test_that("DistributionExponential functionality testing", {
  rates <- c(0.5, 5, 1, 15)

  purrr::walk(rates, function(rate) {
    exp_dist <- DistributionExponential$new(
      rate = rate
    )

    # pdf testing
    x <- c(0.5, 1, 2, 4, 10)
    expect_equal(
      exp_dist$pdf(x),
      dexp(x, rate = rate)
    )

    # cdf testing
    expect_equal(
      exp_dist$cdf(x),
      pexp(x, rate = rate)
    )

    # quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      exp_dist$quantile(q),
      qexp(q, rate = rate)
    )

    # random testing
    expect_equal(
      exp_dist$random(n = 4, seed = 123),
      withr::with_seed(123, rexp(4, rate = rate))
    )
  })
})


### Uniform Distribution testing ----------------------------------------------

test_that("DistributionUniform testing initiation and param checks", {
  expect_error(
    DistributionUniform$new(min = c(1, 2), max = 1),
    "`min` must be a number"
  )
  expect_error(
    DistributionUniform$new(min = "b", max = 1),
    "`min` must be a number"
  )

  expect_error(
    DistributionUniform$new(min = 1, max = c(1, 2)),
    "`max` must be a number"
  )
  expect_error(
    DistributionUniform$new(min = 1, max = "a"),
    "`max` must be a number"
  )
  expect_error(
    DistributionUniform$new(min = 0, max = -1),
    "`max` must be a number larger than or equal to min"
  )
  expect_error(
    DistributionUniform$new(min = 10, max = 5),
    "`max` must be a number larger than or equal to min"
  )
})

test_that("DistributionUniform functionality testing", {
  mins <- c(0, -4, 10, 30)
  maxs <- c(0.5, 3, 15, 1000)

  purrr::walk2(mins, maxs, function(min, max) {
    uniform_dist <- DistributionUniform$new(
      min = min,
      max = max
    )

    # pdf testing
    diff <- max - min
    x <- c(min + diff / 10, min + diff / 4 + min + 3 * diff / 5, min + 99 * diff / 100)
    expect_equal(
      uniform_dist$pdf(x),
      dunif(x, min = min, max = max)
    )

    # cdf testing
    expect_equal(
      uniform_dist$cdf(x),
      punif(x, min = min, max = max)
    )

    # quantile testing
    q <- c(0.1, 0.4, 0.7, 0.95)
    expect_equal(
      uniform_dist$quantile(q),
      qunif(q, min = min, max = max)
    )

    # random testing
    expect_equal(
      uniform_dist$random(n = 4, seed = 123),
      withr::with_seed(123, runif(4, min = min, max = max))
    )
  })
})


### graphing method testing ---------------------------------------------------

test_that("plot method works as expected", {
  normal_dist <- DistributionNormal$new(
    mean = 0,
    sd = 1
  )

  from <- -4
  to <- 4
  n <- 1000
  xlab <- "xtest"
  ylab <- "ytest"
  main <- "main test"

  comp <- plot(dnorm, from, to, n = n)
  p1 <- normal_dist$plot(from = from, to = to, n = n, xlab = xlab, ylab = ylab, main = main)
  p2 <- plot(normal_dist, from = from, to = to, n = n, xlab = xlab, ylab = ylab, main = main)

  expect_equal(
    p1$x,
    comp$x
  )
  expect_equal(
    p1$y,
    comp$y
  )
  expect_equal(
    p1,
    p2
  )

  # parameter checks
  expect_equal(
    p1$x[1],
    from
  )
  expect_equal(
    p1$x[n],
    to
  )
  expect_equal(
    p1$xlab,
    xlab
  )
  expect_equal(
    p1$ylab,
    ylab
  )
  expect_equal(
    p1$main,
    main
  )
})

test_that("snapshot tests for plot method", {
  vdiffr::expect_doppelganger(
    "Normal Distribution graph method",
    DistributionNormal$new(
      mean = 5,
      sd = 2
    )$plot(testing = TRUE)
  )

  vdiffr::expect_doppelganger(
    "Beta Distribution graph method",
    DistributionBeta$new(
      shape1 = 1,
      shape2 = 3
    )$plot(testing = TRUE)
  )

  vdiffr::expect_doppelganger(
    "Gamma Distribution graph method",
    DistributionGamma$new(
      shape = 2,
      rate = 2
    )$plot(testing = TRUE)
  )

  vdiffr::expect_doppelganger(
    "Exponential Distribution graph method",
    DistributionExponential$new(
      rate = 3
    )$plot(testing = TRUE)
  )

  vdiffr::expect_doppelganger(
    "Uniform Distribution graph method",
    out <- DistributionUniform$new(
      min = 0,
      max = 3
    )$plot(testing = TRUE)
  )
})


### print method testing ------------------------------------------------------

test_that("print method works as expected", {
  normal_dist <- DistributionNormal$new(
    mean = 2,
    sd = 1
  )
  expect_output(
    normal_dist$print(),
    "Normal Distribution \\(mean = 2, sd = 1\\)"
  )
  expect_output(
    print(normal_dist),
    "Normal Distribution \\(mean = 2, sd = 1\\)"
  )

  beta_dist <- DistributionBeta$new(
    shape1 = 5,
    shape2 = 2
  )
  expect_output(
    print(beta_dist),
    "Beta Distribution \\(shape1 = 5, shape2 = 2\\)"
  )

  gamma_dist <- DistributionGamma$new(
    shape = 2,
    rate = 5
  )
  expect_output(
    print(gamma_dist),
    "Gamma Distribution \\(shape = 2, rate = 5\\)"
  )

  exp_dist <- DistributionExponential$new(
    rate = 2
  )
  expect_output(
    print(exp_dist),
    "Exponential Distribution \\(rate = 2\\)"
  )

  uniform_dist <- DistributionUniform$new(
    min = 2,
    max = 100
  )
  expect_output(
    print(uniform_dist),
    "Uniform Distribution \\(min = 2, max = 100\\)"
  )
})
