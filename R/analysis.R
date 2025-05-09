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
    simulation_N = 100000,
    add_fixed_cost = function(value, name) {
      check_name(name, arg = name)
      check_number_decimal(value, arg = name)
      self$fixed_costs[[name]] <- value
    },
    add_variable_cost = function(distribution, name) {
      check_name(name, arg = name)
      check_distribution(distribution, arg = name)
      self$variable_costs[[name]] <- distribution
    },
    .evaluate_scenario = function() {
      stop("build out in subclass")
    }
  ),
  public = list(
    run_simulation = function(N = self$simulation_N) {
      sim_results <- purrr::map(seq_len(N), function(i) {
        private$evaluate_scenario()
      }) %>%
        purrr::list_rbind()

      self$sim_results <- sim_results

      invisible(self)
    }
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
    },
    rent = function(value) {
      if (missing(value)) {
        return(self$variable_costs$rent)
      } else {
        check_distribution(value, arg = "rent")
        self$variable_costs$rent <- value
      }
    },
    property_taxes = function(value) {
      if (missing(value)) {
        return(self$variable_costs$property_taxes)
      } else {
        check_distribution(value, arg = "property_taxes")
        self$variable_costs$property_taxes <- value
      }
    },
    insurance = function(value) {
      if (missing(value)) {
        return(self$variable_costs$insurance)
      } else {
        check_distribution(value, arg = "insurance")
        self$variable_costs$insurance <- value
      }
    },
    maintenance = function(value) {
      if (missing(value)) {
        return(self$variable_costs$maintenance)
      } else {
        check_distribution(value, arg = "maintenance")
        self$variable_costs$maintenance <- value
      }
    },
    vacancy = function(value) {
      if (missing(value)) {
        return(self$variable_costs$vacancy)
      } else {
        check_distribution(value, arg = "vacancy")
        self$variable_costs$vacancy <- value
      }
    },
    capital_expenditures = function(value) {
      if (missing(value)) {
        return(self$variable_costs$capital_expenditures)
      } else {
        check_distribution(value, arg = "capital_expenditures")
        self$variable_costs$capital_expenditures <- value
      }
    },
    property_management = function(value) {
      if (missing(value)) {
        return(self$variable_costs$property_management)
      } else {
        check_distribution(value, arg = "property_management")
        self$variable_costs$property_management <- value
      }
    }
  ),
  private = list(
    evaluate_scenario = function() {
      scenario_params <- purrr::map(self$variable_costs, function(dist) {
        dist$random()
      })
    }
  ),
  public = list(
    initialize = function(purchase_price,
                          down_payment,
                          mortgage_payment,
                          rent,
                          property_taxes,
                          insurance,
                          maintenance,
                          vacancy,
                          capital_expenditures,
                          property_management,
                          fixed_monthly_extras = NULL,
                          variable_extras = NULL,
                          simulation_N = 100000) {
      self$purchase_price <- purchase_price
      self$down_payment <- down_payment
      self$mortgage_payment <- mortgage_payment
      self$rent <- rent
      self$property_taxes <- property_taxes
      self$insurance <- insurance
      self$maintenance <- maintenance
      self$vacancy <- vacancy
      self$capital_expenditures <- capital_expenditures
      self$property_management <- property_management

      check_number_whole(simulation_N)
      private$simulation_N <- simulation_N

      check_list(fixed_monthly_extras, allow_null = TRUE)
      purrr::iwalk(fixed_monthly_extras, private$add_fixed_cost)

      check_list(variable_extras, allow_null = TRUE)
      purrr::iwalk(variable_extras, private$add_variable_cost)
    }
  )
)
