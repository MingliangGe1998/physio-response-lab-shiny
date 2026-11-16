sample_data <- function(submitter_key = NA) {
  data.frame(row_id = 1, id = "p01", submitter_key = submitter_key,
             baseline_hr30 = 35, stringsAsFactors = FALSE)
}

test_that("a saved project is loaded again with the creator key", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", c("SUBMIT01", "SUBMIT02"), sample_data(),
                   list(creator_name = "Test Instructor"))

  project <- app$load_project("CREATOR1")
  expect_equal(project$access_type, "creator")
  expect_equal(project$metadata$project_name, "Demo Class")
  expect_equal(project$data, sample_data())
  expect_equal(project$project_info$creator_name, "Test Instructor")
})

test_that("a submitter key opens the project it belongs to", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", c("SUBMIT01", "SUBMIT02"), data.frame())

  project <- app$load_project("SUBMIT02")
  expect_equal(project$access_type, "submitter")
  expect_equal(project$metadata$creator_key, "CREATOR1")
  expect_false(project$has_submitted)
})

test_that("a submitter key that already has a record is marked as used", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", c("SUBMIT01", "SUBMIT02"), sample_data("SUBMIT01"))

  expect_true(app$load_project("SUBMIT01")$has_submitted)
  expect_false(app$load_project("SUBMIT02")$has_submitted)
})

test_that("an unknown key opens nothing", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", "SUBMIT01", data.frame())
  expect_null(app$load_project("NOSUCHKY"))
})

record <- function(id, submitter_key = NA) {
  data.frame(row_id = NA_real_, id = id, submitter_key = submitter_key,
             baseline_hr30 = 35, stringsAsFactors = FALSE)
}

test_that("records are added to what is on disk and numbered after the highest row", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", c("SUBMIT01", "SUBMIT02"), data.frame())

  first <- app$add_records("CREATOR1", record(c("p01", "p02")))
  expect_true(first$added)
  expect_equal(first$data$row_id, c(1, 2))

  app$delete_record("CREATOR1", 1)
  second <- app$add_records("CREATOR1", record("p03", "SUBMIT01"))
  expect_equal(second$data$id, c("p02", "p03"))
  expect_equal(second$data$row_id, c(2, 3))
  expect_equal(app$read_project_data("CREATOR1"), second$data)
  expect_true(app$load_project("SUBMIT01")$has_submitted)
})

test_that("an ID that is already in the project is refused, whatever the letter case", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", "SUBMIT01", data.frame())
  app$add_records("CREATOR1", record("p01"))

  result <- app$add_records("CREATOR1", record(c("P01", "p02")))
  expect_false(result$added)
  expect_equal(result$duplicates, "P01")
  expect_equal(app$read_project_data("CREATOR1")$id, "p01")
})

test_that("the CSV copy follows the data, also when the last record is deleted", {
  local_workdir()
  app$save_project("Demo Class", "CREATOR1", "SUBMIT01", data.frame())
  backup <- file.path("projects", "CREATOR1", "data_backup.csv")

  app$add_records("CREATOR1", record("p01"))
  expect_equal(read.csv(backup)$id, "p01")

  app$delete_record("CREATOR1", 1)
  expect_equal(nrow(read.csv(backup)), 0)
})
