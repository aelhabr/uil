
rm(list = ls())

dir_script <- "R/"
filename_script <- "03-analyze_01-v01"
filepath_script <- paste0(dir_script, filename_script, ".R")

# filename_output <- filename_script
dir_output <- "../"
filename_output <- "uil_01"
filepath_output_ext <- ".html"
filepath_output <- paste0(dir_output, filename_output, filepath_output_ext)

# Uncommente this line to generate the .md file for an html document.
# knitr::spin(filepath_script, knit = FALSE)
render <- TRUE
render_backup <- TRUE

if (render == TRUE) {

  rmarkdown::render(
    input = filepath_script,
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
       filepath_output_backup <- paste0(
         filename_script,
         "_",
         format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
         filepath_output_ext
       )
       file.copy(from = filepath_output, to = filepath_output_backup)
     # }
   }

}
