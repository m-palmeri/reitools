DistributionValidator <- R6::R6Class(
  classname = "DistributionValidator",
  private = list(
    expected_params = list(),

    # check for the right parameter names
    check_names = function(params) {
      assertthat::assert_that(
        setequal(names(params), private$expected_params),
        msg = paste("params should include", toString(private$expected_params))
      )
    },

    # check for the right order of parameters
    check_order = function(params) {
      assertthat::assert_that(
        all(names(params) == private$expected_params),
        msg = paste("params should have the order", toString(private$expected_params))
      )
    },

    # check each parameter is just length one
    check_length = function(params) {
      assertthat::assert_that(
        all(sapply(params, length) == 1),
        msg = "each element of params should be of length 1"
      )
    },

    # check that parameter values are numeric
    check_numeric = function(params) {
      assertthat::assert_that(
        all(sapply(params, is.numeric)),
        msg = "each element of params should be numeric"
      )
    },

    # check that parameter values are valid
    check_valid = function(params) {

    }
  ),
  public = list(
    validate = function(params) {
      private$check_names(params)
      private$check_order(params)
      private$check_length(params)
      private$check_numeric(params)
      private$check_valid(params)
    }
  )
)

NormalDistributionValidator <- R6::R6Class(
  classname = "NormalDistributionValidator",
  inherit = DistributionValidator,
  private = list(
    expected_params = c("mean", "sd"),

    check_valid = function(params) {
      assertthat::assert_that(
        params$sd > 0,
        msg = "sd parameter must be greater than 0"
      )
    }
  )
)

BetaDistributionValidator <- R6::R6Class(
  classname = "BetaDistributionValidator",
  inherit = DistributionValidator,
  private = list(
    expected_params = c("alpha", "beta"),

    check_valid = function(params) {
      assertthat::assert_that(
        params$alpha > 0 && params$beta > 0,
        msg = "alpha and beta parameters must be greater than 0"
      )
    }
  )
)

validate_distribution_params <- function(params, type) {
  validator <- switch(type,
                      normal = NormalDistributionValidator$new(),
                      beta = BetaDistributionValidator$new())
  validator$validate(params)
}
