Scenario <- R6::R6Class(
  classname = "Scenario",
  active = list(
    incomes = function(value) {
      if (missing(value)) {

      } else {
        stop(
          "incomes is read-only. Use add_item, update_item, or remove_item",
          " methods to adjust this"
        )
      }
    },
    expenses = function(value) {
      if (missing(value)) {

      } else {
        stop(
          "expenses is read-only. Use add_item, update_item, or remove_item",
          " methods to adjust this"
        )
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
    add_item <- function(name, value) {
      check_character(name)
      check_number_decimal(value)

      private$items[[name]] <- value
    },
    update_item <- function(name, new_value) {
      check_character(name)
      check_number_decimal(new_value)
      if (!(name %in% names(private$items))) {
        stop_input_type(
          name,
          "",
          arg = rlang::caller_arg(name),
          call = rlang::caller_env()
        )
      }

      private$items[[name]] <- new_value
    },
    remove_item <- function(name) {
      check_character(name)

      private$items[[name]] <- NULL
    }
  )
)
