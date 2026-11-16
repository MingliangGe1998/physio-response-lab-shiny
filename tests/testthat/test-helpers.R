test_that("project keys are 8 letters or digits", {
  keys <- replicate(200, app$generate_project_key())
  expect_true(all(grepl("^[A-Za-z0-9]{8}$", keys)))
})

test_that("column names are cleaned and netid becomes id", {
  expect_equal(
    app$clean_column_names(c(" NetID ", "Duration (in seconds)", "baseline_HR30", "id")),
    c("id", "durationinseconds", "baseline_hr30", "id")
  )
})

test_that("text is converted to numbers and blanks become NA", {
  expect_equal(
    app$safe_numeric_conversion(c("35", " 40 ", "NA", "N/A", "", ".", "abc")),
    c(35, 40, NA, NA, NA, NA, NA)
  )
})

test_that("survey header rows inside the data are found", {
  df <- data.frame(
    id = c("Please enter your ID", '{"ImportId":"QID1"}', "p01", "p02"),
    baseline_hr30 = c("Rate your pulse", '{"ImportId":"QID2"}', "35", "36"),
    stringsAsFactors = FALSE
  )
  expect_equal(app$detect_header_rows(df), c(1L, 2L))
})

test_that("an upload without an ID column is rejected", {
  report <- app$validate_uploaded_data(data.frame(baseline_hr30 = c(35, 36)))
  expect_true(report$has_errors)
})

test_that("valid IDs are counted, empty IDs are flagged", {
  ok <- app$validate_uploaded_data(data.frame(id = c("p01", "p02", "p03"), stringsAsFactors = FALSE))
  expect_false(ok$has_errors)
  expect_equal(ok$valid_ids, 3)

  empty <- app$validate_uploaded_data(data.frame(id = c("p01", "", NA), stringsAsFactors = FALSE))
  expect_true(empty$has_errors)
  expect_equal(empty$valid_ids, 1)
})
