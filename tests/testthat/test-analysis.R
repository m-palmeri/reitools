test_that("Buy and Hold Analysis initialization and active field testing", {
  purchase_price = 100000
  down_payment = 20000
  mortgage_payment = 1000
  rent = DistributionNormal$new(2500, 300)
  property_taxes = DistributionNormal$new(200, 25)
  insurance = DistributionNormal$new(150, 25)
  maintenance = DistributionNormal$new(250, 30)
  vacancy = DistributionBeta$new(2, 18)
  capital_expenditures = DistributionNormal$new(300, 50)
  property_management = DistributionNormal$new(250, 30)

  expect_error(
    AnalysisBH$new(
      purchase_price = "a",
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`purchase_price` must be a number"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = "a",
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`down_payment` must be a number"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = "a",
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`mortgage_payment` must be a number"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = "a",
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`rent` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = "a",
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`property_taxes` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = "a",
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`insurance` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = "a",
      vacancy = vacancy,
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`maintenance` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = "a",
      capital_expenditures = capital_expenditures,
      property_management = property_management
    ),
    "`vacancy` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      rent = rent,
      property_taxes = property_taxes,
      insurance = insurance,
      maintenance = maintenance,
      vacancy = vacancy,
      capital_expenditures = "a",
      property_management = property_management
    ),
    "`capital_expenditures` must be a distribution"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
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
    purchase_price = purchase_price,
    down_payment = down_payment,
    mortgage_payment = mortgage_payment,
    rent = rent,
    property_taxes = property_taxes,
    insurance = insurance,
    maintenance = maintenance,
    vacancy = vacancy,
    capital_expenditures = capital_expenditures,
    property_management = property_management
  )
  expect_equal(
    analysis_test$purchase_price,
    purchase_price
  )
  expect_equal(
    analysis_test$down_payment,
    down_payment
  )
  expect_equal(
    analysis_test$mortgage_payment,
    mortgage_payment
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
    c("purchase_price", "down_payment")
  )
  expect_equal(
    names(analysis_test$monthly_fixed_costs),
    c("mortgage_payment")
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

test_that("AnalysisBH testing onetime_extras, fixed_extras, and variable_extras", {

})
