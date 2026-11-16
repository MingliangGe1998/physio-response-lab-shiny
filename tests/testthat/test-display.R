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
