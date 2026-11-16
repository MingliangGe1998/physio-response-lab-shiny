test_that("the protocol file names the 50 columns of the app, in order", {
  expect_equal(app$protocol$element, app$protocol_columns())
  expect_equal(app$protocol$position, 1:50)
})

test_that("a pulse count is refused, flagged or accepted", {
  expect_equal(
    app$check_values(c(-1, 35.5, 0, 19, 20, 60, 61, NA), "baseline_hr30"),
    c("refuse", "refuse", "flag", "flag", "ok", "ok", "flag", "missing")
  )
})

test_that("a rating is a whole number from 1 to 10", {
  expect_equal(
    app$check_values(c(0, 1, 5.5, 10, 11, NA), "city_stress"),
    c("refuse", "ok", "refuse", "ok", "refuse", "missing")
  )
})

test_that("a cognitive test score is a whole number of at most 80", {
  expect_equal(
    app$check_values(c(81, 80, 79.5, 0, -80, -81, NA), "cognitive_gamescore"),
    c("refuse", "ok", "refuse", "ok", "ok", "flag", "missing")
  )
})

test_that("text is read as a number, and an unknown element is an error", {
  expect_equal(
    app$check_values(c("35", "abc", ""), "baseline_hr30"),
    c("ok", "missing", "missing")
  )
  expect_error(app$check_values(35, "no_such_element"), "no_such_element")
})

test_that("the simulated file holds no value that is refused or flagged", {
  demo <- read.csv(demo_file)
  for (element in app$protocol_columns()) {
    expect_true(all(app$check_values(demo[[element]], element) == "ok"), info = element)
  }
})
