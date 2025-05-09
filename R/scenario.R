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
        Filter(function(x)  x$type == "monthly", private$items) %>%
          purrr::map(\(x) x$value) %>%
          unlist() %>%
          sum()
      } else {
        stop("monthly_profit is read-only.")
      }
    }
  ),
  private = list(
    items = list(),
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
    }
  )
)

