#' ---
#' title: "Exploring Texas school Academic UIL Competition Results"
#' author: "Tony"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    css: markdown7.css
#' ---
#'
#'
#'
#+ include = FALSE
knitr::opts_chunk$set(
  echo = FALSE,
  cache = TRUE,
  fig.align = "center",
  # width = 100,
  warning = FALSE,
  message = FALSE
)


# Need this if render script is not in same directory as project root.
# knitr::opts_knit$set(root.dir = "O:/_other/projects/uil/")
knitr::opts_knit$set(root.dir = rprojroot::find_root("uil.Rproj"))

options(scipen = 1, digits = 2)

# Comment these lines out if calling this script with parameters.
# rm(list = ls())

#'
#'
#'
#+ child = "R/03-analyze_00-v01.Rmd"

#'
#'
#'
#+ child = "R/03-analyze_01-v01.Rmd"

#'
#'
#'
#+ child = "R/03-analyze_02-v01.Rmd"

#'
#'
#'
#'
#+ child = "R/03-analyze_03-v01.Rmd"
#'

