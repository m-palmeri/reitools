Distribution <- R6::R6Class(
  classname = "Distribution",
  active = list(
    params = function(value) {
      if (missing(value)) {
        return(private$.params)
      } else {
        stop("params is read-only. To change this, change the individual parameter fields")
      }
    },
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      } else {
        stop("type is read-only")
      }
    }
  ),
  private = list(
    .params = list(),
    .type = character(),
    pdf_function = NULL,
    cdf_function = NULL,
    quantile_function = NULL,
    randomizer_function = NULL,

    # check each parameter is just length one
    check_length = function(param, name, expected_length = 1) {
      assertthat::assert_that(
        length(param) == expected_length,
        msg = glue::glue("{name} parameter should be of length {expected_length}")
      )
    },

    # check that parameter value is numeric
    check_numeric = function(param, name) {
      assertthat::assert_that(
        is.numeric(param),
        msg = glue::glue("{name} parameter should be numeric")
      )
    },

    # check that parameter value is greater than a minimum value
    check_greater_than = function(param, name, value) {
      assertthat::assert_that(
        param > value,
        msg = glue::glue("{name} parameter must be greater than {value}")
      )
    },

    # check that parameter value is greater than a minimum value
    check_lesser_than = function(param, name, value) {
      assertthat::assert_that(
        param < value,
        msg = glue::glue("{name} parameter must be lesser than {value}")
      )
    },

    # helper method for printing
    .print = function(round_digits = 2) {
      dist_text <- paste0(
        tools::toTitleCase(self$type),
        " Distribution ",
        "(",
        purrr::imap(private$.params, .f = function(x, i) {
          paste(i, "=", round(x, round_digits))
        }) %>%
          unlist() %>%
          toString(),
        ")"
      )
      return(dist_text)
    }
  ),
  public = list(
    pdf = function(x) {
      args <- append(list(x), private$.params)
      value <- do.call(private$pdf_function, args)
      return(value)
    },
    cdf = function(x) {
      args <- append(list(x), private$.params)
      value <- do.call(private$cdf_function, args)
      return(value)
    },
    quantile = function(q) {
      args <- append(list(q), private$.params)
      value <- do.call(private$quantile_function, args)
      return(value)
    },
    random = function(n = 1, seed = 1) {
      args <- append(list(n), private$.params)
      value <- withr::with_seed(seed, do.call(private$randomizer_function, args))
      return(value)
    },

    plot = function(to = NULL,
                    from = NULL,
                    xlim = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    main = NULL,
                    n = 100,
                    testing = FALSE,
                    ...) {
      if (self$type == "beta" && is.null(xlim)) {
        from <- from %||% 0
        to <- to %||% 1
      } else if (is.null(xlim)) {
        from <- from %||% self$quantile(0.001)
        to <- to %||% self$quantile(0.999)
      } else {
        from <- from %||% xlim[1]
        to <- to %||% xlim[2]
      }

      xlab <- xlab %||% "x"
      ylab <- ylab %||% "PDF"
      main <- main %||% private$.print()

      pdf_function <- self$pdf

      temp <- curve(
        expr = pdf_function, from = from, to = to, xlim = xlim,
        xlab = xlab, ylab = ylab, main = main, n = n, ...
      )

      if (testing) {
        return(NULL)
      } else {
        invisible(list(
          x = temp$x,
          y = temp$y,
          xlab = xlab,
          ylab = ylab,
          main = main
        ))
      }
    },
    print = function(...) {
      dist_text <- private$.print(...)

      cat(dist_text)
    }
  )
)

NormalDistribution <- R6::R6Class(
  classname = "NormalDistribution",
  inherit = Distribution,
  active = list(
    mean = function(value) {
      if (missing(value)) {
        return(private$.params$mean)
      } else {
        private$check_length(value, "mean")
        private$check_numeric(value, "mean")
        private$.params$mean <- value
      }
    },
    sd = function(value) {
      if (missing(value)) {
        return(private$.params$sd)
      } else {
        private$check_length(value, "sd")
        private$check_numeric(value, "sd")
        private$check_greater_than(value, "sd", 0)
        private$.params$sd <- value
      }
    }
  ),
  private = list(
    .type = "normal"
  ),
  public = list(
    initialize = function(mean, sd) {
      self$mean <- mean
      self$sd <- sd

      private$pdf_function <- stats::dnorm
      private$cdf_function <- stats::pnorm
      private$quantile_function <- stats::qnorm
      private$randomizer_function <- stats::rnorm
    }
  )
)

BetaDistribution <- R6::R6Class(
  classname = "BetaDistribution",
  inherit = Distribution,
  active = list(
    shape1 = function(value) {
      if (missing(value)) {
        return(private$.params$shape1)
      } else {
        private$check_length(value, "shape1")
        private$check_numeric(value, "shape1")
        private$check_greater_than(value, "shape1", 0)
        private$.params$shape1 <- value
      }
    },
    shape2 = function(value) {
      if (missing(value)) {
        return(private$.params$shape2)
      } else {
        private$check_length(value, "shape2")
        private$check_numeric(value, "shape2")
        private$check_greater_than(value, "shape2", 0)
        private$.params$shape2 <- value
      }
    }
  ),
  private = list(
    .type = "beta"
  ),
  public = list(
    initialize = function(shape1, shape2) {
      self$shape1 <- shape1
      self$shape2 <- shape2

      private$pdf_function <- stats::dbeta
      private$cdf_function <- stats::pbeta
      private$quantile_function <- stats::qbeta
      private$randomizer_function <- stats::rbeta
    }
  )
)

GammaDistribution <- R6::R6Class(
  classname = "GammaDistribution",
  inherit = Distribution,
  active = list(
    shape = function(value) {
      if (missing(value)) {
        return(private$.params$shape)
      } else {
        private$check_length(value, "shape")
        private$check_numeric(value, "shape")
        private$check_greater_than(value, "shape", 0)
        private$.params$shape <- value
      }
    },
    rate = function(value) {
      if (missing(value)) {
        return(private$.params$rate)
      } else {
        private$check_length(value, "rate")
        private$check_numeric(value, "rate")
        private$check_greater_than(value, "rate", 0)
        private$.params$rate <- value
      }
    }
  ),
  private = list(
    .type = "gamma"
  ),
  public = list(
    initialize = function(shape, rate) {
      self$shape <- shape
      self$rate <- rate

      private$pdf_function <- stats::dgamma
      private$cdf_function <- stats::pgamma
      private$quantile_function <- stats::qgamma
      private$randomizer_function <- stats::rgamma
    }
  )
)

ExponentialDistribution <- R6::R6Class(
  classname = "ExponentialDistribution",
  inherit = Distribution,
  active = list(
    rate = function(value) {
      if (missing(value)) {
        return(private$.params$rate)
      } else {
        private$check_length(value, "rate")
        private$check_numeric(value, "rate")
        private$check_greater_than(value, "rate", 0)
        private$.params$rate <- value
      }
    }
  ),
  private = list(
    .type = "exponential"
  ),
  public = list(
    initialize = function(rate) {
      self$rate <- rate

      private$pdf_function <- stats::dexp
      private$cdf_function <- stats::pexp
      private$quantile_function <- stats::qexp
      private$randomizer_function <- stats::rexp
    }
  )
)

UniformDistribution <- R6::R6Class(
  classname = "UniformDistribution",
  inherit = Distribution,
  active = list(
    min = function(value) {
      if (missing(value)) {
        return(private$.params$min)
      } else {
        private$check_length(value, "min")
        private$check_numeric(value, "min")
        private$.params$min <- value
      }
    },
    max = function(value) {
      if (missing(value)) {
        return(private$.params$max)
      } else {
        private$check_length(value, "max")
        private$check_numeric(value, "max")
        private$check_greater_than(value, "max", self$min)
        private$.params$max <- value
      }
    }
  ),
  private = list(
    .type = "uniform"
  ),
  public = list(
    initialize = function(min, max) {
      self$min <- min
      self$max <- max

      private$pdf_function <- stats::dunif
      private$cdf_function <- stats::punif
      private$quantile_function <- stats::qunif
      private$randomizer_function <- stats::runif
    }
  )
)
