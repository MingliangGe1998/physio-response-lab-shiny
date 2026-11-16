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

test_that("the manual form stores nothing when a value is refused, and keeps a flagged one", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01", score = 81)
    expect_equal(nrow(values$projectData), 0)

    enter_participant(session, "p01", count = 65, score = 80)
    expect_equal(nrow(values$projectData), 1)
    expect_true(all(unlist(values$projectData[hr_columns()]) == 65))
  })
})

test_that("an upload with one refused value stores nothing", {
  local_workdir()
  demo <- read.csv(demo_file, check.names = FALSE, colClasses = "character")
  demo$cognitive_gamescore[3] <- "81"
  refused_file <- file.path(getwd(), "one_refused.csv")
  write.csv(demo, refused_file, row.names = FALSE)

  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, refused_file)
    expect_equal(nrow(values$projectData), 0)

    upload_file(session, demo_file)
    expect_equal(nrow(values$projectData), 50)
  })
})

test_that("the message names the refused values, the first ten of them", {
  fine <- data.frame(id = "p01", cognitive_gamescore = 80)
  expect_null(app$refused_message(fine, upload = FALSE))

  one <- data.frame(id = "p01", baseline_hr30 = -1, city_stress = 11, cognitive_gamescore = 81)
  expect_equal(
    app$refused_message(one, upload = FALSE),
    "Not saved. These values cannot be right: baseline_hr30 = -1; city_stress = 11; cognitive_gamescore = 81."
  )
  expect_equal(
    app$refused_message(one[c("id", "cognitive_gamescore")], upload = TRUE),
    "Nothing was uploaded. 1 value cannot be right: p01 cognitive_gamescore = 81."
  )

  many <- data.frame(id = paste0("p", 1:12), prequiz_hr30 = -1, city_stress = c(5, 5.5, rep(5, 10)))
  expect_equal(
    app$refused_message(many, upload = TRUE),
    paste0(
      "Nothing was uploaded. 13 values cannot be right: ",
      paste0("p", 1:10, " prequiz_hr30 = -1", collapse = "; "),
      "; and 3 more."
    )
  )
})
