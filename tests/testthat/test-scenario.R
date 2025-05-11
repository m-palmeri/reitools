test_that("testing initialization and active fields", {
  test1 <- Scenario$new(
    monthly_items = list(
      "income1" = 150,
      "expense2" = -300,
      "expense1" = -50,
      "income2" = 500,
      "income3" = 200
    ),
    onetime_items = list(
      "onetime1" = 100000,
      "onetime2" = 2500
    )
  )
  expect_equal(
    names(test1$monthly_incomes),
    c("income1", "income2", "income3")
  )
  expect_equal(
    names(test1$monthly_expenses),
    c("expense2", "expense1")
  )
  expect_equal(
    names(test1$onetime_items),
    c("onetime1", "onetime2")
  )
  expect_equal(
    test1$monthly_profit,
    500
  )
})


test_that("add_item, update_item, and remove_item method testing", {
  test1 <- make_fake_scenario()

  # add_item checks
  expect_error(
    test1$add_item(name = 1, value = 1, type = "monthly"),
    "`name` must be a single string"
  )
  expect_error(
    test1$add_item(name = "a", value = "a", type = "monthly"),
    "`value` must be a number"
  )
  expect_error(
    test1$add_item(name = "a", value = 1, type = 2),
    "`type` must be a single string"
  )
  expect_error(
    test1$add_item(name = "income1", value = 1, type = "a"),
    "`name` must not be already defined"
  )
  test1$add_item("testing1", 50, "monthly")
  test1$add_item("testing2", -100, "monthly")
  test1$add_item("testing3", 10000, "one-time")
  expect_contains(
    test1$monthly_incomes,
    list("testing1" = 50)
  )
  expect_contains(
    test1$monthly_expenses,
    list("testing2" = -100)
  )
  expect_contains(
    test1$onetime_items,
    list("testing3" = 10000)
  )

  # update_item checks
  expect_error(
    test1$update_item(name = 1, new_value = 1),
    "`name` must be a single string"
  )
  expect_error(
    test1$update_item(name = "testing1", new_value = "a"),
    "`new_value` must be a number"
  )
  expect_error(
    test1$update_item(name = "test", new_value = 1),
    "`name` must be already defined"
  )
  test1$update_item("testing1", 150)
  test1$update_item("testing2", 25)
  expect_contains(
    test1$monthly_incomes,
    list(
      "testing1" = 150,
      "testing2" = 25
    )
  )

  # remove_item checks
  expect_error(
    test1$remove_item(name = 1),
    "`name` must be a single string"
  )
  expect_error(
    test1$remove_item(name = "test"),
    "`name` must be already defined"
  )
  test1$remove_item("testing1")
  test1$remove_item("testing2")
  expect_false(
    "testing1" %in% names(test1$.__enclos_env__$private$items)
  )
  expect_false(
    "testing2" %in% names(test1$.__enclos_env__$private$items)
  )
})
