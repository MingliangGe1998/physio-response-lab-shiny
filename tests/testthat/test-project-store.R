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
