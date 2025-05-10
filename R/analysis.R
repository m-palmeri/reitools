Analysis <- R6::R6Class(
  classname = "Analysis",
  active = list(
    onetime_costs = function(value) {
      if (missing(value)) {
        return(private$.onetime_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(onetime_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(onetime_costs)[{i}]"))
          check_number_decimal(value[[i]], arg = glue::glue("onetime_costs${name}"))
        })
        private$.onetime_costs <- value
      }
    },
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
    .onetime_costs = list(),
    .fixed_costs = list(),
    .variable_costs = list(),
    simulation_N = 100000,
    add_onetime_cost = function(value, name) {
      check_name(name, arg = name)
      check_number_decimal(value, arg = name)
      self$onetime_costs[[name]] <- value
    },
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
    evaluate_scenario = function() {
      stop("build out in subclass")
    }
  ),
  public = list(
    simulation_results = NULL,
    run_simulation = function(N = NULL) {
      N <- N %||% private$simulation_N
      sim_results <- purrr::map(seq_len(N), function(i) {
        private$evaluate_scenario()
      }) |>
        purrr::list_rbind()

      self$simulation_results <- sim_results

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
        return(self$onetime_costs$purchase_price)
      } else {
        private$add_onetime_cost(value, "purchase_price")
      }
    },
    down_payment = function(value) {
      if (missing(value)) {
        return(self$onetime_costs$down_payment)
      } else {
        private$add_onetime_cost(value, "down_payment")
      }
    },
    mortgage_payment = function(value) {
      if (missing(value)) {
        return(self$fixed_costs$mortgage_payment)
      } else {
        private$add_fixed_cost(value, "mortgage_payment")
      }
    },
    rent = function(value) {
      if (missing(value)) {
        return(self$variable_costs$rent)
      } else {
        private$add_variable_cost(value, "rent")
      }
    },
    property_taxes = function(value) {
      if (missing(value)) {
        return(self$variable_costs$property_taxes)
      } else {
        private$add_variable_cost(value, "property_taxes")
      }
    },
    insurance = function(value) {
      if (missing(value)) {
        return(self$variable_costs$insurance)
      } else {
        private$add_variable_cost(value, "insurance")
      }
    },
    maintenance = function(value) {
      if (missing(value)) {
        return(self$variable_costs$maintenance)
      } else {
        private$add_variable_cost(value, "maintenance")
      }
    },
    vacancy = function(value) {
      if (missing(value)) {
        return(self$variable_costs$vacancy)
      } else {
        private$add_variable_cost(value, "vacancy")
      }
    },
    capital_expenditures = function(value) {
      if (missing(value)) {
        return(self$variable_costs$capital_expenditures)
      } else {
        private$add_variable_cost(value, "capital_expenditures")
      }
    },
    property_management = function(value) {
      if (missing(value)) {
        return(self$variable_costs$property_management)
      } else {
        private$add_variable_cost(value, "property_management")
      }
    }
  ),
  private = list(
    evaluate_scenario = function() {
      scenario_params <- purrr::map(self$variable_costs, function(dist) {
        dist$random()
      })

      if (self$vacancy$type == "beta") { #vacancy done as a percent of rent
        scenario_params$vacancy <- -scenario_params$vacancy * scenario_params$rent
      }
      if (self$property_management$type == "beta") { #property_management done as a percent of rent
        scenario_params$property_management <- -scenario_params$property_management * scenario_params$rent
      }

      all_onetime_items <- list(
        purchase_price = self$purchase_price,
        down_payment = self$down_payment
      )
      all_monthly_items <- c(
        self$fixed_costs,
        scenario_params
      )

      scenario <- ScenarioBH$new(
        monthly_items = all_monthly_items,
        onetime_items = all_onetime_items
      )

      results <- scenario$make_results()

      return(results)
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
                          onetime_extras = NULL,
                          fixed_extras = NULL,
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

      check_list(onetime_extras, allow_null = TRUE)
      purrr::iwalk(onetime_extras, private$add_onetime_cost)

      check_list(fixed_extras, allow_null = TRUE)
      purrr::iwalk(fixed_extras, private$add_fixed_cost)

      check_list(variable_extras, allow_null = TRUE)
      purrr::iwalk(variable_extras, private$add_variable_cost)
    }
  )
)
