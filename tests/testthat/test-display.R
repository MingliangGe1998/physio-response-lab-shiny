# The cells of a table widget as they are sent to the browser.
rendered_cells <- function(widget) widget$preRenderHook(widget)$x$data

test_that("the creator's table shows a participant ID as typed and keeps the Delete button", {
  data <- data.frame(row_id = 1, id = "<em>p01", timestamp = Sys.time(), stringsAsFactors = FALSE)

  # columns sent: row name, row_id, id, timestamp, Actions
  cells <- rendered_cells(app$uploaded_data_table(data, can_delete = TRUE))
  expect_equal(cells[[3]], "&lt;em&gt;p01")
  expect_match(cells[[5]], "^<button onclick=")

  cells <- rendered_cells(app$uploaded_data_table(data, can_delete = FALSE))
  expect_equal(cells[[5]], "N/A")
})

test_that("the report file name holds only letters, digits, hyphens and underscores from the ID", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "../p 01")
    session$setInputs(reportParticipant = "../p 01")
    click(session, "generateReport")
    expect_match(basename(output$downloadReport), "^___p_01_PhysiologyReport_")
  })
})

home_page <- function() {
  html <- NULL
  shiny::testServer(app$server, {
    html <<- as.character(output$mainUI$html)
  })
  html
}

test_that("the home page shows a document button only when the file is in docs/", {
  local_workdir()
  expect_false(grepl("downloadProtocol", home_page(), fixed = TRUE))
  expect_false(grepl("downloadGuide", home_page(), fixed = TRUE))

  dir.create("docs")
  writeLines("protocol", app$protocol_file)
  expect_true(grepl("downloadProtocol", home_page(), fixed = TRUE))
  expect_false(grepl("downloadGuide", home_page(), fixed = TRUE))

  shiny::testServer(app$server, {
    expect_equal(readLines(output$downloadProtocol), "protocol")
  })
})

test_that("the home page has no link that leads nowhere", {
  local_workdir()
  expect_false(grepl('href="#"', home_page(), fixed = TRUE))
})

test_that("the file upload button is offered to creators only, and is not disabled", {
  local_workdir()
  project <- NULL
  shiny::testServer(app$server, {
    create_project(session)
    project <<- values$currentProject
    choice <- as.character(output$uploadInterface$html)
    button <- regmatches(choice, regexpr('<button[^>]*id="selectFile"[^>]*>', choice))
    expect_length(button, 1)
    expect_false(grepl("disabled", button, fixed = TRUE))
  })
  shiny::testServer(app$server, {
    open_project(session, project$submitter_keys[1])
    choice <- as.character(output$uploadInterface$html)
    expect_false(grepl('id="selectFile"', choice, fixed = TRUE))
    expect_true(grepl("File upload is only available for creators", choice, fixed = TRUE))
  })
})
