Analysis <- R6::R6Class(
  classname = "Analysis",
  active = list(
    onetime_fixed_costs = function(value) {
      if (missing(value)) {
        return(private$.onetime_fixed_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(onetime_fixed_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(onetime_fixed_costs)[{i}]"))
          check_number_decimal(value[[i]], arg = glue::glue("onetime_fixed_costs${name}"))
        })
        private$.onetime_fixed_costs <- value
      }
    },
    onetime_variable_costs = function(value) {
      if (missing(value)) {
        return(private$.onetime_variable_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(onetime_variable_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(onetime_variable_costs)[{i}]"))
          check_distribution(value[[i]], arg = glue::glue("onetime_variable_costs${name}"))
        })
        private$.onetime_variable_costs <- value
      }
    },
    monthly_fixed_costs = function(value) {
      if (missing(value)) {
        return(private$.monthly_fixed_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(monthly_fixed_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(monthly_fixed_costs)[{i}]"))
          check_number_decimal(value[[i]], arg = glue::glue("monthly_fixed_costs${name}"))
        })
        private$.monthly_fixed_costs <- value
      }
    },
    monthly_variable_costs = function(value) {
      if (missing(value)) {
        return(private$.monthly_variable_costs)
      } else {
        check_list(value)
        check_character(names(value), arg = "names(monthly_variable_costs)")
        purrr::walk(seq_along(value), function(i) {
          name <- names(value)[i]
          check_name(name, arg = glue::glue("names(monthly_variable_costs)[{i}]"))
          check_distribution(value[[i]], arg = glue::glue("monthly_variable_costs${name}"))
        })
        private$.monthly_variable_costs <- value
      }
    }
  ),
  private = list(
    .onetime_fixed_costs = list(),
    .onetime_variable_costs = list(),
    .monthly_fixed_costs = list(),
    .monthly_variable_costs = list(),
    simulation_N = 100000,
    add_onetime_fixed_cost = function(value, name) {
      check_name(name, arg = name)
      check_number_decimal(value, arg = name)
      self$onetime_fixed_costs[[name]] <- value
    },
    add_onetime_variable_cost = function(value, name) {
      check_name(name, arg = name)
      check_distribution(value, arg = name)
      self$onetime_variable_costs[[name]] <- value
    },
    add_monthly_fixed_cost = function(value, name) {
      check_name(name, arg = name)
      check_number_decimal(value, arg = name)
      self$monthly_fixed_costs[[name]] <- value
    },
    add_monthly_variable_cost = function(distribution, name) {
      check_name(name, arg = name)
      check_distribution(distribution, arg = name)
      self$monthly_variable_costs[[name]] <- distribution
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
        return(self$onetime_fixed_costs$purchase_price)
      } else {
        private$add_onetime_fixed_cost(value, "purchase_price")
      }
    },
    down_payment = function(value) {
      if (missing(value)) {
        return(self$onetime_fixed_costs$down_payment)
      } else {
        private$add_onetime_fixed_cost(value, "down_payment")
      }
    },
    mortgage_payment = function(value) {
      if (missing(value)) {
        return(self$monthly_fixed_costs$mortgage_payment)
      } else {
        private$add_monthly_fixed_cost(value, "mortgage_payment")
      }
    },
    rent = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$rent)
      } else {
        private$add_monthly_variable_cost(value, "rent")
      }
    },
    property_taxes = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$property_taxes)
      } else {
        private$add_monthly_variable_cost(value, "property_taxes")
      }
    },
    insurance = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$insurance)
      } else {
        private$add_monthly_variable_cost(value, "insurance")
      }
    },
    maintenance = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$maintenance)
      } else {
        private$add_monthly_variable_cost(value, "maintenance")
      }
    },
    vacancy = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$vacancy)
      } else {
        private$add_monthly_variable_cost(value, "vacancy")
      }
    },
    capital_expenditures = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$capital_expenditures)
      } else {
        private$add_monthly_variable_cost(value, "capital_expenditures")
      }
    },
    property_management = function(value) {
      if (missing(value)) {
        return(self$monthly_variable_costs$property_management)
      } else {
        private$add_monthly_variable_cost(value, "property_management")
      }
    }
  ),
  private = list(
    evaluate_scenario = function() {
      onetime_params <- purrr::map(self$onetime_variable_costs, function(dist) {
        dist$random()
      })

      month_params <- purrr::map(self$monthly_variable_costs, function(dist) {
        dist$random()
      })

      if (self$vacancy$type == "beta") { #vacancy done as a percent of rent
        month_params$vacancy <- -month_params$vacancy * month_params$rent
      }
      if (self$property_management$type == "beta") { #property_management done as a percent of rent
        month_params$property_management <- -month_params$property_management * month_params$rent
      }

      all_onetime_items <- c(
        self$onetime_fixed_costs,
        onetime_params
      )
      all_monthly_items <- c(
        self$monthly_fixed_costs,
        month_params
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
                          onetime_fixed_extras = NULL,
                          onetime_variable_extras = NULL,
                          monthly_fixed_extras = NULL,
                          monthly_variable_extras = NULL,
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

      check_list(onetime_fixed_extras, allow_null = TRUE)
      purrr::iwalk(onetime_fixed_extras, private$add_onetime_fixed_cost)

      check_list(onetime_variable_extras, allow_null = TRUE)
      purrr::iwalk(onetime_variable_extras, private$add_onetime_variable_cost)

      check_list(monthly_fixed_extras, allow_null = TRUE)
      purrr::iwalk(monthly_fixed_extras, private$add_monthly_fixed_cost)

      check_list(monthly_variable_extras, allow_null = TRUE)
      purrr::iwalk(monthly_variable_extras, private$add_monthly_variable_cost)
    }
  )
)
