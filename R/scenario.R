Scenario <- R6::R6Class(
  classname = "Scenario",
  active = list(
    incomes = function(value) {
      if (missing(value)) {
        Filter(function(x) x > 0, private$items)
      } else {
        stop("incomes is read-only. Use add_item, update_item, or remove_item",
             " methods to adjust this")
      }
    },
    expenses = function(value) {
      if (missing(value)) {
        Filter(function(x) x < 0, private$items)
      } else {
        stop("expenses is read-only. Use add_item, update_item, or remove_item",
             " methods to adjust this")
      }
    },
    profit = function(value) {
      if (missing(value)) {
        sum(unlist(private$items))
      } else {
        stop("cash_flow is read-only.")
      }
    }
  ),
  private = list(
    items = list()
  ),
  public = list(
    initialize = function(...) {
      dots <- list(...)

      # assigning input to incomes/expenses
      purrr::iwalk(dots, function(x, name) {
        self$add_item(name, x)
      })
    },

    add_item = function(name, value) {
      check_string(name)
      check_number_decimal(value)
      if (name %in% names(private$items)) {
        rlang::abort(sprintf(
          "`name` must not be an already defined expense or income. \n    -%s \n    -%s",
          paste("defined incomes:", toString(names(self$incomes))),
          paste("defined expenses:", toString(names(self$expenses)))
        ))
      }

      private$items[[name]] <- value
    },

    update_item = function(name, new_value) {
      check_string(name)
      check_number_decimal(new_value)
      if (!(name %in% names(private$items))) {
        rlang::abort(sprintf(
          "`name` must be an already defined expense or income. \n    -%s \n    -%s",
          paste("defined incomes:", toString(names(self$incomes))),
          paste("defined expenses:", toString(names(self$expenses)))
        ))
      }

      private$items[[name]] <- new_value
    },

    remove_item = function(name) {
      check_string(name)
      if (!(name %in% names(private$items))) {
        rlang::abort(sprintf(
          "`name` must be an already defined expense or income. \n    -%s \n    -%s",
          paste("defined incomes:", toString(names(self$incomes))),
          paste("defined expenses:", toString(names(self$expenses)))
        ))
      }

      private$items[[name]] <- NULL
    }
  )
)

