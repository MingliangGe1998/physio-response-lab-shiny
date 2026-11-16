# One test for each defect found in the audit before release. Every test
# here failed on the 2025 classroom version of app.R and passes now.

test_that("every notification uses a type that Shiny accepts", {
  # showNotification() stops with an error for any other type, and an error
  # inside an observer ends the user's session.
  allowed <- eval(formals(shiny::showNotification)$type)

  types <- character()
  collect <- function(expr) {
    if (!is.call(expr)) return(invisible())
    if (identical(expr[[1]], as.name("showNotification"))) {
      types <<- c(types, if (is.null(expr$type)) "default" else as.character(expr$type))
    }
    for (i in seq_along(expr)) {
      part <- expr[[i]]
      if (!missing(part)) collect(part)
    }
  }
  for (expr in parse(app_file)) collect(expr)

  expect_gt(length(types), 0)
  expect_equal(setdiff(types, allowed), character())
})

test_that("two submitters with the project open at the same time are both saved", {
  local_workdir()
  project <- NULL
  shiny::testServer(app$server, {
    create_project(session)
    project <<- values$currentProject
  })

  rscript <- file.path(R.home("bin"), "Rscript")
  script <- file.path(app_dir, "tests", "testthat", "scripts", "submit_one.R")
  shiny::testServer(app$server, {
    open_project(session, project$submitter_keys[2])          # student 2 opens the project
    status <- system2(rscript, shQuote(c(script, app_file, getwd(), project$submitter_keys[1], "student-1")),
                      stdout = FALSE, stderr = FALSE)          # student 1 opens it and submits
    expect_equal(status, 0)
    expect_equal(rows_on_disk(project$creator_key), 1)
    enter_participant(session, "student-2")                    # student 2 submits
  })

  saved <- app$read_project_data(project$creator_key)
  expect_setequal(saved$id, c("student-1", "student-2"))
})

test_that("a change made by the creator keeps records submitted since the project was opened", {
  local_workdir()
  rscript <- file.path(R.home("bin"), "Rscript")
  script <- file.path(app_dir, "tests", "testthat", "scripts", "submit_one.R")
  shiny::testServer(app$server, {
    create_project(session)                                    # the creator has the project open
    project <- values$currentProject
    status <- system2(rscript, shQuote(c(script, app_file, getwd(), project$submitter_keys[1], "student-1")),
                      stdout = FALSE, stderr = FALSE)          # a student submits
    expect_equal(status, 0)
    enter_participant(session, "late-entry")                   # the creator adds a record by hand

    saved <- app$read_project_data(project$creator_key)
    expect_setequal(saved$id, c("student-1", "late-entry"))
  })
})

test_that("a deleted record does not come back when the project is opened again", {
  local_workdir()
  key <- NULL
  shiny::testServer(app$server, {
    create_project(session)
    key <<- values$currentProject$creator_key
    enter_participant(session, "p01")
    session$setInputs(deleteRow = 1)
  })
  shiny::testServer(app$server, {
    open_project(session, key)
    expect_equal(nrow(values$projectData), 0)
  })
})

test_that("a participant can be entered by hand after a survey file was uploaded", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    enter_participant(session, "late-student")
    expect_equal(nrow(values$projectData), 51)
  })
})

test_that("a survey file can be uploaded after a participant was entered by hand", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "early-student")
    upload_file(session, demo_file)
    expect_equal(nrow(values$projectData), 51)
  })
})

test_that("deleting one row removes exactly one participant", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    for (id in c("a", "b", "c")) enter_participant(session, id)
    session$setInputs(deleteRow = 1)
    enter_participant(session, "d")
    expect_equal(anyDuplicated(values$projectData$row_id), 0)

    session$setInputs(deleteRow = max(values$projectData$row_id))
    expect_equal(nrow(values$projectData), 2)
  })
})

test_that("the statistical test table has three columns", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    header <- html_table_rows(output$heartRateStats)[[1]]
    expect_equal(header, c("Section", "Mean Difference (BPM)", "P-value"))
  })
})

test_that("pulse counts in the demo file are 30-second counts", {
  demo <- read.csv(demo_file)
  counts <- unlist(demo[hr_columns()])
  # The usual range of a 30-second count in the protocol is 20 to 60.
  expect_equal(sum(counts < 20 | counts > 60, na.rm = TRUE), 0)
})

test_that("ratings in the demo file are on the 1 to 10 scale", {
  demo <- read.csv(demo_file)
  ratings <- unlist(demo[affect_columns()])
  expect_equal(sum(ratings < 1 | ratings > 10, na.rm = TRUE), 0)
})

test_that("identifying survey columns are not kept or exported", {
  local_workdir()
  identifying <- c("ipaddress", "recipientlastname", "recipientfirstname", "recipientemail",
                   "externalreference", "locationlatitude", "locationlongitude", "responseid")
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    expect_equal(intersect(names(values$projectData), identifying), character())
    expect_equal(intersect(names(read.csv(output$downloadAllData)), identifying), character())
  })
})

test_that("a participant ID is escaped in the individual report", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "<em>p01")
    session$setInputs(reportParticipant = "<em>p01")
    click(session, "generateReport")
    report <- paste(readLines(output$downloadReport), collapse = "\n")
    expect_false(grepl("<em>p01", report, fixed = TRUE))
  })
})

test_that("a submitter can submit when the project already holds records from the creator", {
  local_workdir()
  from_creator <- data.frame(row_id = 1, id = "p01", timestamp = Sys.time(), submitter_key = NA,
                             baseline_hr30 = 35, stringsAsFactors = FALSE)
  app$save_project("Demo Class", "CREATOR1", c("SUBMIT01", "SUBMIT02"), from_creator)

  shiny::testServer(app$server, {
    open_project(session, "SUBMIT01")
    screen <- as.character(output$uploadInterface$html)
    expect_false(grepl("Data Already Submitted", screen, fixed = TRUE))
    expect_true(grepl('id="selectManual"', screen, fixed = TRUE))
  })
})

test_that("the individual report declares its character set and is plain ASCII around the data", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01")
    session$setInputs(reportParticipant = "p01")
    click(session, "generateReport")
    report <- paste(readLines(output$downloadReport, warn = FALSE), collapse = "\n")
    expect_true(grepl('<meta charset="utf-8">', report, fixed = TRUE))
    # with an ASCII participant ID nothing in the report needs another encoding
    expect_false(grepl("[^\\x01-\\x7F]", report, perl = TRUE, useBytes = TRUE))
  })
})
