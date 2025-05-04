test_that("testing initialization and active fields", {
  test1 <- make_fake_scenario()

  expect_equal(
    names(test1$incomes),
    c("income1", "income2", "income3")
  )

  expect_equal(
    names(test1$expenses),
    c("expense2", "expense1")
  )
})


test_that("add_item, update_item, and remove_item method testing", {
  test1 <- make_fake_scenario()

  # add_item checks
  expect_error(
    test1$add_item(name = 1, value = 1),
    "`name` must be a single string"
  )
  expect_error(
    test1$add_item(name = "a", value = "a"),
    "`value` must be a number"
  )
  expect_error(
    test1$add_item(name = "income1", value = 1),
    "`name` must not be an already defined expense or income"
  )
  test1$add_item("testing1", 50)
  test1$add_item("testing2", -100)
  expect_contains(
    test1$incomes,
    list("testing1" = 50)
  )
  expect_contains(
    test1$expenses,
    list("testing2" = -100)
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
    "`name` must be an already defined expense or income"
  )
  test1$update_item("testing1", 150)
  test1$update_item("testing2", 25)
  expect_contains(
    test1$incomes,
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
    "`name` must be an already defined expense or income"
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
