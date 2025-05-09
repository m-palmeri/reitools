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
    names(analysis_test$fixed_costs),
    c("purchase_price", "down_payment", "mortgage_payment")
  )
  expect_equal(
    names(analysis_test$variable_costs),
    c("vacancy", "rent")
  )
})
