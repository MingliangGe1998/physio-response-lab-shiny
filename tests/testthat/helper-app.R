# Shared setup for all tests.
# Loads the objects defined in app.R (helpers, ui, server) without starting a
# server, and provides small drivers for the server-side workflow.

app_dir   <- normalizePath(file.path("..", ".."), mustWork = TRUE)
app_file  <- file.path(app_dir, "app.R")
demo_file <- file.path(app_dir, "data", "simulated_bio3_data.csv")

app <- new.env(parent = globalenv())
# chdir: app.R reads the protocol file from its own folder when it is loaded.
suppressPackageStartupMessages(sys.source(app_file, envir = app, chdir = TRUE))

hr_columns <- function() paste0(app$section_prefixes, "_hr30")

affect_columns <- function() {
  as.vector(t(outer(app$section_prefixes, names(app$affect_measures), paste, sep = "_")))
}

# The app stores projects in ./projects relative to the working directory,
# so every test gets its own empty working folder.
local_workdir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  invisible(dir)
}

# Action buttons only fire when their value changes.
.clicks <- new.env()
click <- function(session, id) {
  n <- get0(id, envir = .clicks, ifnotfound = 0) + 1
  assign(id, n, envir = .clicks)
  do.call(session$setInputs, stats::setNames(list(n), id))
}

create_project <- function(session, participants = 3) {
  session$setInputs(
    creatorName = "Test Instructor",
    creatorEmail = "instructor@example.org",
    institution = "Example College",
    expectedParticipants = as.character(participants),
    experimentStartDate = Sys.Date(),
    experimentEndDate = Sys.Date() + 7,
    className = "Demo Class",
    schoolLevel = "college",
    projectDescription = ""
  )
  click(session, "confirmCreate")
  click(session, "continueToWorkspace")
}

open_project <- function(session, key) {
  session$setInputs(recallKey = key)
  click(session, "confirmRecall")
}

# Fill the manual entry form for one participant and press "Save Data".
# `count` is the 30-second pulse count used for every section.
enter_participant <- function(session, id, count = 35, rating = 5, score = 80) {
  values <- list(participantID = id, cognitive_gamescore_input = score)
  for (prefix in app$section_prefixes) {
    values[[paste0(prefix, "_hr30_input")]] <- count
    for (measure in names(app$affect_measures)) {
      values[[paste0(prefix, "_", measure, "_input")]] <- rating
    }
  }
  do.call(session$setInputs, values)
  click(session, "saveManualData")
}

upload_file <- function(session, path) {
  click(session, "selectFile")
  session$setInputs(uploadFile = data.frame(
    name = basename(path),
    size = file.size(path),
    type = "text/csv",
    datapath = path,
    stringsAsFactors = FALSE
  ))
}

rows_on_disk <- function(creator_key) {
  data <- readRDS(file.path("projects", creator_key, "data.rds"))
  if (is.null(data)) 0L else nrow(data)
}

# Cells of an HTML table as a list of character vectors, one per row.
html_table_rows <- function(html) {
  rows <- regmatches(html, gregexpr("<tr[^>]*>.*?</tr>", html, perl = TRUE))[[1]]
  lapply(rows, function(row) {
    cells <- regmatches(row, gregexpr("<t[dh][^>]*>.*?</t[dh]>", row, perl = TRUE))[[1]]
    trimws(gsub("<[^>]+>", "", cells))
  })
}
