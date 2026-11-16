test_that("creating a project writes it to disk with one key per participant", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session, participants = 4)

    key <- values$currentProject$creator_key
    expect_match(key, "^[A-Za-z0-9]{8}$")
    expect_length(values$currentProject$submitter_keys, 4)
    expect_equal(values$accessType, "creator")
    expect_equal(values$currentState, "workspace")
    expect_setequal(
      list.files(file.path("projects", key)),
      c("data.rds", "metadata.rds", "project_info.rds", "submitter_usage.rds")
    )
  })
})

test_that("manual entry stores the 30-second counts as entered", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01", count = 35, rating = 7, score = 91)

    expect_equal(nrow(values$projectData), 1)
    expect_equal(values$projectData$id, "p01")
    expect_true(all(unlist(values$projectData[hr_columns()]) == 35))
    expect_true(all(unlist(values$projectData[affect_columns()]) == 7))
    expect_equal(values$projectData$cognitive_gamescore, 91)
  })
})

test_that("a participant ID cannot be entered twice, whatever the letter case", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01")
    enter_participant(session, "P01")
    expect_equal(nrow(values$projectData), 1)
  })
})

test_that("records entered by hand are on disk at once and come back with the creator key", {
  local_workdir()
  key <- NULL
  shiny::testServer(app$server, {
    create_project(session)
    key <<- values$currentProject$creator_key
    enter_participant(session, "p01")
    enter_participant(session, "p02")
    expect_equal(rows_on_disk(key), 2)
  })
  shiny::testServer(app$server, {
    open_project(session, key)
    expect_equal(values$accessType, "creator")
    expect_equal(values$projectData$id, c("p01", "p02"))
  })
})

test_that("Refresh Data shows the records that others have submitted", {
  local_workdir()
  rscript <- file.path(R.home("bin"), "Rscript")
  script <- file.path(app_dir, "tests", "testthat", "scripts", "submit_one.R")
  shiny::testServer(app$server, {
    create_project(session)
    project <- values$currentProject
    status <- system2(rscript, shQuote(c(script, app_file, getwd(), project$submitter_keys[1], "student-1")),
                      stdout = FALSE, stderr = FALSE)
    expect_equal(status, 0)
    expect_equal(nrow(values$projectData), 0)
    click(session, "refreshProject")
    expect_equal(values$projectData$id, "student-1")
  })
})

test_that("an unknown key leaves the app on the home page", {
  local_workdir()
  shiny::testServer(app$server, {
    open_project(session, "NOSUCHKY")
    expect_equal(values$currentState, "home")
    expect_null(values$currentProject)
  })
})

test_that("a submitter key saves one record at once and cannot be used again", {
  local_workdir()
  project <- NULL
  shiny::testServer(app$server, {
    create_project(session)
    project <<- values$currentProject
  })
  shiny::testServer(app$server, {
    open_project(session, project$submitter_keys[1])
    expect_equal(values$accessType, "submitter")
    enter_participant(session, "student-1")
  })
  expect_equal(rows_on_disk(project$creator_key), 1)

  shiny::testServer(app$server, {
    open_project(session, project$submitter_keys[1])
    # The "already submitted" dialog is shown and the workspace is not opened.
    expect_equal(values$currentState, "home")
  })
})

test_that("the demo file uploads as 50 participants and is not added twice", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    expect_equal(nrow(values$projectData), 50)
    expect_equal(length(unique(values$projectData$id)), 50)
    expect_true(is.numeric(values$projectData$baseline_hr30))

    again <- file.path(getwd(), "again.csv")
    file.copy(demo_file, again)
    upload_file(session, again)
    expect_equal(nrow(values$projectData), 50)
  })
})

test_that("extra header rows from a survey export are dropped on upload", {
  local_workdir()
  demo <- read.csv(demo_file, check.names = FALSE, colClasses = "character")
  question_row <- stats::setNames(as.list(paste("Rate your", names(demo))), names(demo))
  import_row <- stats::setNames(as.list(sprintf('{"ImportId":"QID%d"}', seq_along(demo))), names(demo))
  survey_file <- file.path(getwd(), "survey_export.csv")
  write.csv(rbind(question_row, import_row, demo), survey_file, row.names = FALSE)

  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, survey_file)
    expect_equal(nrow(values$projectData), 50)
    expect_true(is.numeric(values$projectData$baseline_hr30))
  })
})

test_that("the individual report and the full export are produced", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    participant <- values$projectData$id[1]

    session$setInputs(reportParticipant = participant)
    click(session, "generateReport")
    report <- readLines(output$downloadReport)
    expect_true(any(grepl(participant, report, fixed = TRUE)))
    expect_true(any(grepl("Class Average", report, fixed = TRUE)))

    exported <- read.csv(output$downloadAllData)
    expect_equal(nrow(exported), 50)
    expect_true(all(c("id", hr_columns(), affect_columns(), "cognitive_gamescore") %in% names(exported)))
    expect_false(any(c("row_id", "submitter_key") %in% names(exported)))
  })
})

test_that("every screen renders", {
  local_workdir()
  project <- NULL
  shiny::testServer(app$server, {
    expect_error(output$mainUI, NA)                            # home page
    create_project(session)
    project <<- values$currentProject
    expect_error(output$uploadInterface, NA)                   # choice of entry method
    click(session, "selectManual")
    expect_error(output$uploadInterface, NA)                   # manual entry form
    upload_file(session, demo_file)
    expect_error(output$uploadInterface, NA)                   # file upload form
    click(session, "stopAddingData")
    session$setInputs(affectMeasure = "stress")
    for (name in c("mainUI", "uploadInterface", "uploadedDataTable", "dataPreview", "completionOverview",
                   "heartRatePreview", "affectPreview", "statSummary", "dataAnalysis", "heartRatePlot",
                   "heartRateStats", "affectPlot", "reportGeneration")) {
      expect_error(output[[name]], NA, info = name)            # creator workspace with data
    }
  })
  shiny::testServer(app$server, {
    open_project(session, project$submitter_keys[1])
    for (name in c("mainUI", "uploadInterface", "limitedDataPreview")) {
      expect_error(output[[name]], NA, info = name)            # submitter workspace
    }
  })
})

test_that("closing a project leaves no output in an error state", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01")
    click(session, "closeProject")
    expect_equal(values$currentState, "home")
    for (name in c("uploadInterface", "dataAnalysis", "reportGeneration")) {
      # req() stops the output without an error message
      expect_error(output[[name]], class = "shiny.silent.error", info = name)
    }
  })
})
