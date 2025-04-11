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
    randomizer = NULL,

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
    random = function(n = 1) {
      value <- private$randomizer(n, unlist(private$.params))
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
    type = "normal",
    randomizer = rnorm
  ),
  public = list(
    initialize = function(mean, sd) {
      self$mean <- mean
      self$sd <- sd
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
    type = "beta",
    randomizer = rbeta
  ),
  public = list(
    initialize = function(shape1, shape2) {
      self$shape1 <- shape1
      self$shape2 <- shape2
    }
  )
)
