Distribution <- R6::R6Class(
  classname = "Distribution",
  active = list(
    params = function(value) {
      if (missing(value)) {
        return(private$.params)
      } else {
        stop("params is read-only. To change this, change the individual parameter fields")
      }
    }
  ),
  private = list(
    .params = list(),
    type = character(),
    pdf_function = NULL,
    cdf_function = NULL,
    quantile_function = NULL,
    randomizer_function = NULL,

    # check each parameter is just length one
    check_length = function(param, name) {
      assertthat::assert_that(
        length(param) == 1,
        msg = glue::glue("{name} parameter should be of length 1")
      )
    },

    # check that parameter values are numeric
    check_numeric = function(param, name) {
      assertthat::assert_that(
        is.numeric(param),
        msg = glue::glue("{name} parameter should be numeric")
      )
    },

    # check that parameter values are valid
    check_positive = function(param, name) {
      assertthat::assert_that(
        param > 0,
        msg = glue::glue("{name} parameter must be greater than 0")
      )
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
        private$check_positive(value, "sd")
        private$.params$sd <- value
      }
    }
  ),
  private = list(
    type = "normal"
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
        private$check_positive(value, "shape1")
        private$.params$shape1 <- value
      }
    },
    shape2 = function(value) {
      if (missing(value)) {
        return(private$.params$shape2)
      } else {
        private$check_length(value, "shape2")
        private$check_numeric(value, "shape2")
        private$check_positive(value, "shape2")
        private$.params$shape2 <- value
      }
    }
  ),
  private = list(
    type = "beta"
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
