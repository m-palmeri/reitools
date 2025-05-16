test_that("Buy and Hold Analysis initialization and active field testing", {
  financing = Mortgage$new(
    purchase_price = 100000,
    down_payment = 20000,
    interest_rate = 0.07,
    loan_term = 30
  )
  rent = DistributionNormal$new(2500, 300)
  property_taxes = DistributionNormal$new(-200, 25)
  insurance = DistributionNormal$new(-150, 25)
  maintenance = DistributionNormal$new(-250, 30)
  vacancy = DistributionBeta$new(2, 18)
  capital_expenditures = DistributionNormal$new(-300, 50)
  property_management = DistributionNormal$new(-250, 30)

  expect_error(
    AnalysisBH$new(
      financing = "a",
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`financing` must be a financing object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = "a",
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`rent` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = "a",
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`property_taxes` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = property_taxes,
      insurance = "a",
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`insurance` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = "a",
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`maintenance` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = "a",
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`vacancy` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = "a",
      property_management = property_management
    ),
    "`capital_expenditures` must be a distribution object"
  )
  expect_error(
    AnalysisBH$new(
      financing = financing,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = "a"
    ),
    "`property_management` must be a distribution"
  )

  analysis_test <- AnalysisBH$new(
    financing = financing,
    rent = rent,
    property_taxes = property_taxes,
    insurance = insurance,
    maintenance = maintenance,
    vacancy = vacancy,
    capital_expenditures = capital_expenditures,
    property_management = property_management
  )
  expect_equal(
    analysis_test$onetime_fixed_costs$down_payment,
    financing$down_payment
  )
  expect_equal(
    analysis_test$monthly_fixed_costs$monthly_payment,
    -financing$monthly_payment
  )
  expect_equal(
    analysis_test$rent,
    rent
  )
  expect_equal(
    analysis_test$property_taxes,
    property_taxes
  )
  expect_equal(
    analysis_test$insurance,
    insurance
  )
  expect_equal(
    analysis_test$maintenance,
    maintenance
  )
  expect_equal(
    analysis_test$vacancy,
    vacancy
  )
  expect_equal(
    analysis_test$capital_expenditures,
    capital_expenditures
  )
  expect_equal(
    analysis_test$property_management,
    property_management
  )
  expect_equal(
    names(analysis_test$onetime_fixed_costs),
    c("down_payment")
  )
  expect_equal(
    names(analysis_test$monthly_fixed_costs),
    c("monthly_payment")
  )
  expect_equal(
    names(analysis_test$monthly_variable_costs),
    c("rent", "property_taxes", "insurance", "maintenance", "vacancy",
      "capital_expenditures", "property_management")
  )
})

test_that("AnalysisBH run_simulation testing", {
  analysis_test <- make_fake_analysis(type = "AnalysisBH")

  analysis_test$run_simulation(N = 10)
  simulation_results <- analysis_test$simulation_results

  nn <- c(names(analysis_test$monthly_fixed_costs), names(analysis_test$monthly_variable_costs))
  manual_monthly_profit <- apply(simulation_results[nn], 1, sum)
  expect_equal(
    manual_monthly_profit,
    simulation_results$monthly_profit
  )

  manual_annual_roi <- manual_monthly_profit * 12 / simulation_results$down_payment
  expect_equal(
    manual_annual_roi,
    simulation_results$annual_roi
  )
})

test_that("AnalysisBH testing extras variables", {
  financing = Mortgage$new(
    purchase_price = 300000,
    down_payment = 60000,
    interest_rate = 0.07,
    loan_term = 30
  )
  rent = DistributionNormal$new(2500, 300)
  property_taxes = DistributionNormal$new(200, 25)
  insurance = DistributionNormal$new(150, 25)
  maintenance = DistributionNormal$new(250, 30)
  vacancy = DistributionBeta$new(2, 18)
  capital_expenditures = DistributionNormal$new(300, 50)
  property_management = DistributionNormal$new(250, 30)
  onetime_fixed_extras = list(
    "ofe_test1" = 10000,
    "ofe_test2" = -1000
  )
  onetime_variable_extras = list(
    "ove_test1" = DistributionExponential$new(5),
    "ove_test2" = DistributionUniform$new(1, 5),
    "ove_test3" = DistributionNormal$new(10, 1)
  )
  monthly_fixed_extras = list(
    "mfe_test1" = -250,
    "mfe_test2" = 1000
  )
  monthly_variable_extras = list(
    "mve_test1" = DistributionExponential$new(3),
    "mve_test2" = DistributionNormal$new(250, 20)
  )

  analysis_test <- AnalysisBH$new(
    financing = financing,
    rent = rent,
    property_taxes = property_taxes,
    insurance = insurance,
    maintenance = maintenance,
    vacancy = vacancy,
    capital_expenditures = capital_expenditures,
    property_management = property_management,
    onetime_fixed_extras = onetime_fixed_extras,
    onetime_variable_extras = onetime_variable_extras,
    monthly_fixed_extras = monthly_fixed_extras,
    monthly_variable_extras = monthly_variable_extras
  )

  expect_contains(
    names(analysis_test$onetime_fixed_costs),
    c("ofe_test1", "ofe_test2")
  )
  expect_equal(
    analysis_test$onetime_fixed_costs$ofe_test1,
    onetime_fixed_extras$ofe_test1
  )
  expect_equal(
    analysis_test$onetime_fixed_costs$ofe_test2,
    onetime_fixed_extras$ofe_test2
  )

  expect_contains(
    names(analysis_test$onetime_variable_costs),
    c("ove_test1", "ove_test2", "ove_test3")
  )
  expect_equal(
    analysis_test$onetime_variable_costs$ove_test1,
    onetime_variable_extras$ove_test1
  )
  expect_equal(
    analysis_test$onetime_variable_costs$ove_test2,
    onetime_variable_extras$ove_test2
  )
  expect_equal(
    analysis_test$onetime_variable_costs$ove_test3,
    onetime_variable_extras$ove_test3
  )

  expect_contains(
    names(analysis_test$monthly_fixed_costs),
    c("mfe_test1", "mfe_test2")
  )
  expect_equal(
    analysis_test$monthly_fixed_costs$mfe_test1,
    monthly_fixed_extras$mfe_test1
  )
  expect_equal(
    analysis_test$monthly_fixed_costs$mfe_test2,
    monthly_fixed_extras$mfe_test2
  )

  expect_contains(
    names(analysis_test$monthly_variable_costs),
    c("mve_test1", "mve_test2")
  )
  expect_equal(
    analysis_test$monthly_variable_costs$mve_test1,
    monthly_variable_extras$mve_test1
  )
  expect_equal(
    analysis_test$monthly_variable_costs$mve_test2,
    monthly_variable_extras$mve_test2
  )
})

test_that("AnalysisBH find_ideal_offer method works as expected", {
  analysis_test <- make_fake_analysis(type = "AnalysisBH")

  #debug(analysis_test$find_ideal_price)
  expect_error(
    analysis_test$find_ideal_price(test = list("a")),
    "`test` parameter is not a calculated field"
  )
  expect_error(
    analysis_test$find_ideal_price(test1 = list("a"), test2 = list("b")),
    "`test1` and `test2` parameters are not calculated fields"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(a = 1)),
    "`monthly_profit` parameter has bad list names.*'a'"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(a = 1, b = 2)),
    "`monthly_profit` parameter has bad list names.*'a' and 'b'"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(min = "a", percent = 0.5)),
    "`monthly_profit\\$min` must be a number"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(min = 1, percent = "a")),
    "`monthly_profit\\$percent` must be a number"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(min = 1, percent = -0.5)),
    "`monthly_profit\\$percent` must be a number between 0 and 1"
  )
  expect_error(
    analysis_test$find_ideal_price(monthly_profit = list(min = 1, percent = 2)),
    "`monthly_profit\\$percent` must be a number between 0 and 1"
  )
})
