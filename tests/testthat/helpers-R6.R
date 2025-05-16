### Helpers for Scenario testing

make_fake_scenario <- function(monthly_income_count = 3,
                               monthly_expense_count = 2,
                               one_time_item_count = 5,
                               seed = 123) {
  monthly_items <- withr::with_seed(seed, {
    income_list <- lapply(1:monthly_income_count, \(x) round(runif(1, 1, 1000)))
    names(income_list) <- paste0("income", 1:monthly_income_count)

    expense_list <- lapply(monthly_expense_count:1, \(x) -round(runif(1, 1, 1000)))
    names(expense_list) <- paste0("expense", monthly_expense_count:1)

    c(income_list, expense_list)
  })

  one_time_items <- withr::with_seed(seed, {
    temp <- lapply(1:one_time_item_count, \(x) round(runif(1, 1, 500000)))
    names(temp) <- paste0("onetime", 1:one_time_item_count)
    temp
  })

  scenario <- Scenario$new(
    monthly_items = monthly_items,
    onetime_items = one_time_items
  )
  return(scenario)
}


### Helpers for Analysis testing

make_fake_analysis <- function(type,
                               ...) {
  dots <- list(...)
  all_args <- list(
    financing = dots$financing %||% Mortgage$new(
      purchase_price = 400000,
      down_payment = 80000,
      interest_rate = 0.07,
      loan_term = 30
    ),
    rent = dots$rent %||% DistributionNormal$new(2500, 300),
    property_taxes = dots$property_taxes %||% DistributionNormal$new(-200, 25),
    insurance = dots$insurance %||% DistributionNormal$new(-150, 25),
    maintenance = dots$maintenance %||% DistributionNormal$new(-250, 30),
    vacancy = dots$vacancy %||% DistributionBeta$new(2, 18),
    capital_expenditures = dots$capital_expenditures %||% DistributionNormal$new(-300, 50),
    property_management = dots$property_management %||% DistributionNormal$new(-250, 30)
  )

  extra_arg_names <- setdiff(names(dots), names(all_args))
  extra_args <- dots[extra_arg_names]

  dist_class <- switch(
    type,
    "AnalysisBH" = AnalysisBH
  )

  initialize_args <- formals(dist_class$public_methods$initialize)
  nec_arg_names <- Filter(is.symbol, initialize_args) |> names()

  if (type == "AnalysisBH") {
    dist_class <- AnalysisBH
    nec_args <- c(
      "financing", "rent", "property_taxes", "insurance", "maintenance", "vacancy",
      "capital_expenditure", "property_management"
    )
  }

  nec_args <- c(
    all_args[nec_arg_names],
    extra_args
  )

  analysis <- do.call(dist_class$new, nec_args)

  return(analysis)
}
