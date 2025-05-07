test_that("Buy and Hold Analysis initialization and active field testing", {
  purchase_price = 100000
  down_payment = 20000
  mortgage_payment = 1000

  expect_error(
    AnalysisBH$new(
      purchase_price = "a",
      down_payment = down_payment,
      mortgage_payment = mortgage_payment
    ),
    "`purchase_price` must be a number"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = "a",
      mortgage_payment = mortgage_payment
    ),
    "`down_payment` must be a number"
  )
  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = "a"
    ),
    "`mortgage_payment` must be a number"
  )

  expect_error(
    AnalysisBH$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      mortgage_payment = mortgage_payment,
      test = "a"
    ),
    "`test` must be a distribution"
  )

  analysis_test <- AnalysisBH$new(
    purchase_price = purchase_price,
    down_payment = down_payment,
    mortgage_payment = mortgage_payment,
    vacancy = DistributionBeta$new(1, 1),
    rent = DistributionNormal$new(2000, 200)
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
