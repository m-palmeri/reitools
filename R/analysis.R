Analysis <- R6::R6Class(
  classname = "Analysis",
  active = list(
    fixed_costs = function(value) {
      if (missing(value)) {
        return(private$.fixed_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(fixed_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(fixed_costs)[{i}]"))
          check_number_decimal(value[[i]], arg = glue::glue("fixed_costs${name}"))
        })
        private$.fixed_costs <- value
      }
    },
    variable_costs = function(value) {
      if (missing(value)) {
        return(private$.variable_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(variable_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(variable_costs)[{i}]"))
          check_distribution(value[[i]], arg = glue::glue("variable_costs${name}"))
        })
        private$.variable_costs <- value
      }
    }
  ),
  private = list(
    .fixed_costs = list(),
    .variable_costs = list(),
    add_variable_cost = function(distribution, name) {
      check_name(name, arg = name)
      check_distribution(distribution, arg = name)
      self$variable_costs[[name]] <- distribution
    }
  ),
  public = list(

  )
)

AnalysisBH <- R6::R6Class(
  classname = "AnalysisBH",
  inherit = Analysis,
  active = list(
    purchase_price = function(value) {
      if (missing(value)) {
        return(self$fixed_costs$purchase_price)
      } else {
        check_number_decimal(value, arg = "purchase_price")
        self$fixed_costs$purchase_price <- value
      }
    },
    down_payment = function(value) {
      if (missing(value)) {
        return(self$fixed_costs$down_payment)
      } else {
        check_number_decimal(value, arg = "down_payment")
        self$fixed_costs$down_payment <- value
      }
    },
    mortgage_payment = function(value) {
      if (missing(value)) {
        return(self$fixed_costs$mortgage_payment)
      } else {
        check_number_decimal(value, arg = "mortgage_payment")
        self$fixed_costs$mortgage_payment <- value
      }
    }
  ),
  private = list(
    simulation_N = numeric()
  ),
  public = list(
    initialize = function(purchase_price,
                          down_payment,
                          mortgage_payment,
                          ...,
                          simulation_N = 100000) {
      self$purchase_price <- purchase_price
      self$down_payment <- down_payment
      self$mortgage_payment <- mortgage_payment

      check_number_whole(simulation_N)
      private$simulation_N <- simulation_N

      dots <- list(...)
      purrr::iwalk(dots, private$add_variable_cost)
    }
  )
)
