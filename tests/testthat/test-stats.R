# The numbers the app shows are recomputed here directly from the demo file.

demo <- read.csv(demo_file)

test_that("the summary shows beats per minute as twice the 30-second count", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    summary_text <- output$statSummary

    for (i in seq_along(app$section_prefixes)) {
      bpm <- demo[[paste0(app$section_prefixes[i], "_hr30")]] * 2
      expected <- sprintf("%-25s %4d  %6.1f  %6.1f  %6.0f  %6.0f",
                          app$experiment_sections[i], length(bpm), mean(bpm), sd(bpm), min(bpm), max(bpm))
      expect_true(grepl(expected, summary_text, fixed = TRUE), info = app$experiment_sections[i])
    }
  })
})

test_that("each section is compared with baseline by a paired Wilcoxon test", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    upload_file(session, demo_file)
    rows <- html_table_rows(output$heartRateStats)

    baseline <- demo$baseline_hr30 * 2
    for (i in which(app$experiment_sections != "Baseline")) {
      section <- demo[[paste0(app$section_prefixes[i], "_hr30")]] * 2
      difference <- round(mean(section) - mean(baseline), 1)
      p <- wilcox.test(baseline, section, paired = TRUE, exact = FALSE)$p.value
      p_text <- if (p < 0.001) "< 0.001" else sprintf("%.3f", p)

      row <- Filter(function(cells) length(cells) > 0 && cells[1] == app$experiment_sections[i], rows)
      expect_length(row, 1)
      cells <- row[[1]][-1]
      shown <- suppressWarnings(as.numeric(cells))
      expect_true(any(abs(shown - difference) < 1e-6, na.rm = TRUE), info = app$experiment_sections[i])
      expect_true(gsub("&lt;", "<", p_text) %in% gsub("&lt;", "<", cells), info = app$experiment_sections[i])
    }
  })
})

test_that("statistical tests need at least three participants", {
  local_workdir()
  shiny::testServer(app$server, {
    create_project(session)
    enter_participant(session, "p01")
    enter_participant(session, "p02")
    expect_match(output$heartRateStats, "Need at least 3 participants")
  })
})
