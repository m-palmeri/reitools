### Helpers for Scenario testing

make_fake_scenario <- function(incomes = 3, expenses = 2, seed = 123) {
  items <- withr::with_seed(seed, {
    income_list <- lapply(1:incomes, \(x) round(runif(1, 1, 1000)))
    names(income_list) <- paste0("income", 1:incomes)

    expense_list <- lapply(expenses:1, \(x) -round(runif(1, 1, 1000)))
    names(expense_list) <- paste0("expense", expenses:1)

    append(income_list, expense_list)
  })

  scenario <- do.call(Scenario$new, items)
  return(scenario)
}
