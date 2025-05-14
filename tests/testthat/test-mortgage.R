test_that("Initialization and basic active fields work", {
  purchase_price <- 400000
  down_payment <- 80000
  interest_rate <- 0.07
  loan_term <- 30

  expect_error(
    Mortgage$new(
      purchase_price = "a",
      down_payment = down_payment,
      interest_rate = interest_rate,
      loan_term = loan_term
    ),
    "`purchase_price` must be a number"
  )
  expect_error(
    Mortgage$new(
      purchase_price = purchase_price,
      down_payment = "a",
      interest_rate = interest_rate,
      loan_term = loan_term
    ),
    "`down_payment` must be a number"
  )
  expect_error(
    Mortgage$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      interest_rate = "a",
      loan_term = loan_term
    ),
    "`interest_rate` must be a number"
  )
  expect_error(
    Mortgage$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      interest_rate = 7,
      loan_term = loan_term
    ),
    "`interest_rate` must be a number between 0 and 1"
  )
  expect_error(
    Mortgage$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      interest_rate = interest_rate,
      loan_term = "a"
    ),
    "`loan_term` must be a whole number"
  )
  expect_error(
    Mortgage$new(
      purchase_price = purchase_price,
      down_payment = down_payment,
      interest_rate = interest_rate,
      loan_term = 12.5
    ),
    "`loan_term` must be a whole number"
  )

  mortgage <- Mortgage$new(
    purchase_price = purchase_price,
    down_payment = down_payment,
    interest_rate = interest_rate,
    loan_term = loan_term
  )

  expect_error(
    mortgage$amortization_table <- "a",
    "amortization_table is read-only"
  )
  expect_error(
    mortgage$monthly_payment <- "a",
    "monthly_payment is read-only"
  )

  expect_equal(mortgage$purchase_price, purchase_price)
  expect_equal(mortgage$down_payment, down_payment)
  expect_equal(mortgage$interest_rate, interest_rate)
  expect_equal(mortgage$loan_term, loan_term)
})

test_that("monthly_payment is calculated properly", {
  expect_equal(
    Mortgage$new(
      purchase_price = 500000,
      down_payment = 50000,
      interest_rate = 0.075,
      loan_term = 30
    )$monthly_payment,
    3146.47
  )
  expect_equal(
    Mortgage$new(
      purchase_price = 500000,
      down_payment = 50000,
      interest_rate = 0.05,
      loan_term = 30
    )$monthly_payment,
    2415.70
  )
  expect_equal(
    Mortgage$new(
      purchase_price = 250000,
      down_payment = 50000,
      interest_rate = 0.05,
      loan_term = 30
    )$monthly_payment,
    1073.64
  )
  expect_equal(
    Mortgage$new(
      purchase_price = 1000000,
      down_payment = 350000,
      interest_rate = 0.0375,
      loan_term = 20
    )$monthly_payment,
    3853.77
  )
  expect_equal(
    Mortgage$new(
      purchase_price = 1000000,
      down_payment = 150000,
      interest_rate = 0.065,
      loan_term = 20
    )$monthly_payment,
    6337.37
  )
  expect_equal(
    Mortgage$new(
      purchase_price = 700000,
      down_payment = 175000,
      interest_rate = 0.0725,
      loan_term = 30
    )$monthly_payment,
    3581.43
  )
})

test_that("amortization_table is calculated properly", {
  mortgage <- Mortgage$new(
    purchase_price = 700000,
    down_payment = 175000,
    interest_rate = 0.0725,
    loan_term = 30
  )

  amort_table <- mortgage$amortization_table

  expect_equal(
    round(sum(amort_table$interest_payment)),
    764313
  )
  expect_equal(
    round(sum(amort_table$principal_payment)),
    525000
  )
  expect_equal(
    round(sum(amort_table$total_payment)),
    1289313
  )

  mortgage <- Mortgage$new(
    purchase_price = 400000,
    down_payment = 80000,
    interest_rate = 0.04,
    loan_term = 15
  )

  amort_table <- mortgage$amortization_table

  expect_equal(
    round(sum(amort_table$interest_payment)),
    106060
  )
  expect_equal(
    round(sum(amort_table$principal_payment)),
    320000
  )
  expect_equal(
    round(sum(amort_table$total_payment)),
    426060
  )
})
