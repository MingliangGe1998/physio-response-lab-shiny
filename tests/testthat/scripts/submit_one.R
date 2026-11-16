# Opens the project as one submitter in a separate R process and submits one
# record. Used to test two submitters who have the project open at the same time.
# Usage: Rscript submit_one.R <app.R> <working dir> <submitter key> <participant id>
args <- commandArgs(trailingOnly = TRUE)
setwd(args[2])
app <- new.env(parent = globalenv())
suppressPackageStartupMessages(sys.source(args[1], envir = app))
shiny::testServer(app$server, {
  session$setInputs(recallKey = args[3])
  session$setInputs(confirmRecall = 1)
  values <- list(participantID = args[4], cognitive_gamescore_input = 80)
  for (prefix in app$section_prefixes) {
    values[[paste0(prefix, "_hr30_input")]] <- 35
    for (measure in names(app$affect_measures)) {
      values[[paste0(prefix, "_", measure, "_input")]] <- 5
    }
  }
  do.call(session$setInputs, values)
  session$setInputs(saveManualData = 1)
})
