Scenario <- R6::R6Class(
  classname = "Scenario",
  active = list(
    monthly_incomes = function(value) {
      if (missing(value)) {
        Filter(function(x) x$value > 0 && x$type == "monthly", private$items) |>
          purrr::map(\(x) x$value)
      } else {
        stop("monthly_incomes is read-only. Use add_item, update_item, or remove_item",
             " methods to adjust this")
      }
    },
    monthly_expenses = function(value) {
      if (missing(value)) {
        Filter(function(x)  x$value < 0 && x$type == "monthly", private$items) |>
          purrr::map(\(x) x$value)
      } else {
        stop("monthly expenses is read-only. Use add_item, update_item, or remove_item",
             " methods to adjust this")
      }
    },
    onetime_items = function(value) {
      if (missing(value)) {
        Filter(function(x)  x$type == "one-time", private$items) |>
          purrr::map(\(x) x$value)
      } else {
        stop("onetime_items is read-only. Use add_item, update_item, or remove_item",
             " methods to adjust this")
      }
    },
    monthly_profit = function(value) {
      if (missing(value)) {
        Filter(function(x)  x$type == "monthly", private$items) |>
          purrr::map(\(x) x$value) |>
          unlist() |>
          sum()
      } else {
        stop("monthly_profit is read-only.")
      }
    }
  ),
  private = list(
    items = list(),
    calculated_fields = character(),
    .print = function(front = "Scenario defined by the following:") {
      sprintf(
        "%s \n    -%s \n    -%s \n    -%s",
        front,
        paste("defined monthly incomes:", toString(names(self$monthly_incomes))),
        paste("defined monthly expenses:", toString(names(self$monthly_expenses))),
        paste("defined one-time items:", toString(names(self$onetime_items)))
      )
    }
  ),
  public = list(
    initialize = function(monthly_items = NULL,
                          onetime_items = NULL) {
      check_list(monthly_items, allow_null = TRUE)
      check_list(onetime_items, allow_null = TRUE)

      purrr::iwalk(monthly_items, function(x, name) {
        self$add_item(name, x, "monthly")
      })
      purrr::iwalk(onetime_items, function(x, name) {
        self$add_item(name, x, "one-time")
      })
    },

    print = function() {
      print_statement <- private$.print()

      cat(print_statement)
    },

    add_item = function(name, value, type) {
      check_string(name)
      check_number_decimal(value)
      check_string(type)
      if (name %in% names(private$items)) {
        rlang::abort(private$.print("`name` must not be already defined."))
      }

      private$items[[name]] <- list(value = value, type = type)
    },

    update_item = function(name, new_value) {
      check_string(name)
      check_number_decimal(new_value)
      if (!(name %in% names(private$items))) {
        rlang::abort(private$.print("`name` must be already defined."))
      }

      private$items[[name]]$value <- new_value
    },

    remove_item = function(name) {
      check_string(name)
      if (!(name %in% names(private$items))) {
        rlang::abort(private$.print("`name` must be already defined."))
      }

      private$items[[name]] <- NULL
    },

    make_results = function() {
      calculated_list <- purrr::map(private$calculated_fields, function(x) self[[x]]) |>
        rlang::set_names(private$calculated_fields)
      result_list <- c(
        self$onetime_items,
        self$monthly_incomes,
        self$monthly_expenses,
        calculated_list
      ) |>
        as.data.frame.list()
    }
  )
)

ScenarioBH <- R6::R6Class(
  classname = "ScenarioBH",
  inherit = Scenario,
  active = list(
    annual_roi = function(value) {
      if (missing(value)) {
        annual_cash_flow <- self$monthly_profit * 12
        investment <- self$onetime_items
        investment$purchase_price <- NULL
        investment <- sum(unlist(investment))
        roi <- annual_cash_flow / investment
      } else {
        stop("annual_roi is read-only")
      }
    }
  ),
  private = list(
    calculated_fields = c("monthly_profit", "annual_roi")
  )
)

