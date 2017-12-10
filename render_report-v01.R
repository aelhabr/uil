
rm(list = ls())

dir_report <- ""
filename_report <- "report-v01"
filepath_report <- paste0(dir_report, filename_report, ".R")

dir_script <- "R/"
filename_script_prefix <- "03-analyze_0"
filename_script_suffix <- "-v01"
# filepath_script <- paste0(dir_script, filename_script, ".R")

# filename_output <- filename_script
dir_output <- "../"
filename_output <- "report"
filepath_output_ext <- ".html"
filepath_output <- paste0(dir_output, filename_output, filepath_output_ext)
timestamp_backup <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# These are unique for building a report from multiple scripts.
i_max <- 3
test <- FALSE

# Uncommente this line to generate the .md file for an html document.
# knitr::spin(filepath_script, knit = FALSE)
render <- TRUE
manually <- FALSE
render_backup <- TRUE

if(test == TRUE) {
  i <- 0
  # i <- 3
  while(i <= i_max) {
    filepath_script <- paste0(dir_script, filename_script_prefix, i, filename_script_suffix, ".R")
    rmarkdown::render(input = filepath_script)
    i <- i + 1
  }
}

if (render == TRUE) {

  i <- 0
  while(i <= i_max) {
    filepath_script <- paste0(dir_script, filename_script_prefix, i, filename_script_suffix, ".R")
    knitr::spin(filepath_script, knit = FALSE)
    i <- i + 1
  }

  if(manually == TRUE) {
    knitr::spin(filepath_report, knit = FALSE)
  } else {
    rmarkdown::render(
      input = filepath_report,
      # This is the preferred option because it makes specifying a backup
      # filename/filepath easy.
      output_file = filepath_output
      # use only outputer_dir if name does not matter.
      # The output file will re-use the name of the script.
      # This cannot be used in combination with render_backup.
      # output_dir = "",
      # intermediates_dir = "",
      # Use this to overwrite the script's yaml.
      # output_format = "pdf_document"
      # params = list(),
    )

    # This won't work if rm(list = ls()) is called in the .R script.
    if (render_backup == TRUE) {
      # if(file.exists(filepath_output)) {
      filepath_output_backup <- gsub(filepath_output_ext, paste0("_", timestamp_now, filepath_output_ext), filepath_report)
      # filepath_output_backup <- paste0(
      #   filename_report,
      #   "_",
      #   timestamp_backup,
      #   filepath_output_ext
      # )
      file.copy(from = filepath_output, to = filepath_output_backup)
      # }
    }
  }
}
