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

    # helper method for printing
    .print = function(round_digits = 2) {
      check_number_whole(round_digits)
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
    random = function(n = 1, seed = NULL) {
      check_number_whole(n)
      check_number_decimal(seed)
      args <- append(list(n), private$.params)
      value <-ifelse(
        is.null(seed),
        do.call(private$randomizer_function, args),
        withr::with_seed(seed, do.call(private$randomizer_function, args))
      )
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
      check_number_decimal(to, allow_null = TRUE)
      check_number_decimal(from, allow_null = TRUE)
      if (!is.null(xlim) && length(xlim) != 2) {
        stop_input_type(
          xlim,
          "a numeric vector of length 2",
          arg = rlang::caller_arg(x),
          call = rlang::caller_env()
        )
      }
      check_number_decimal(xlim[1], allow_null = TRUE)
      check_number_decimal(xlim[2], allow_null = TRUE)
      check_string(xlab, allow_null = TRUE)
      check_string(ylab, allow_null = TRUE)
      check_string(main, allow_null = TRUE)
      check_number_whole(n, min = 50)
      check_bool(testing)

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

DistributionNormal <- R6::R6Class(
  classname = "DistributionNormal",
  inherit = Distribution,
  active = list(
    mean = function(value) {
      if (missing(value)) {
        return(private$.params$mean)
      } else {
        check_number_decimal(value, arg = "mean")
        private$.params$mean <- value
      }
    },
    sd = function(value) {
      if (missing(value)) {
        return(private$.params$sd)
      } else {
        check_number_decimal(value, min = 0, arg = "sd")
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

      invisible(self)
    }
  )
)

DistributionBeta <- R6::R6Class(
  classname = "DistributionBeta",
  inherit = Distribution,
  active = list(
    shape1 = function(value) {
      if (missing(value)) {
        return(private$.params$shape1)
      } else {
        check_number_decimal(value, min = 0, arg = "shape1")
        private$.params$shape1 <- value
      }
    },
    shape2 = function(value) {
      if (missing(value)) {
        return(private$.params$shape2)
      } else {
        check_number_decimal(value, min = 0, arg = "shape2")
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

      invisible(self)
    }
  )
)

DistributionGamma <- R6::R6Class(
  classname = "DistributionGamma",
  inherit = Distribution,
  active = list(
    shape = function(value) {
      if (missing(value)) {
        return(private$.params$shape)
      } else {
        check_number_decimal(value, min = 0, arg = "shape")
        private$.params$shape <- value
      }
    },
    rate = function(value) {
      if (missing(value)) {
        return(private$.params$rate)
      } else {
        check_number_decimal(value, min = 0, arg = "rate")
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

      invisible(self)
    }
  )
)

DistributionExponential <- R6::R6Class(
  classname = "DistributionExponential",
  inherit = Distribution,
  active = list(
    rate = function(value) {
      if (missing(value)) {
        return(private$.params$rate)
      } else {
        check_number_decimal(value, min = 0, arg = "rate")
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

      invisible(self)
    }
  )
)

DistributionUniform <- R6::R6Class(
  classname = "DistributionUniform",
  inherit = Distribution,
  active = list(
    min = function(value) {
      if (missing(value)) {
        return(private$.params$min)
      } else {
        check_number_decimal(value, arg = "min")
        private$.params$min <- value
      }
    },
    max = function(value) {
      if (missing(value)) {
        return(private$.params$max)
      } else {
        check_number_decimal(value, min = self$min, arg = "max", min_override = "min")
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

      invisible(self)
    }
  )
)
