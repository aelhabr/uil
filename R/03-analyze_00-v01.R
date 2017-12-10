
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
# Packages. ----
library("dplyr")
library("stringr")
library("tidyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("printr")


theme_set(theme_minimal())

# library("printr")
#'
#'
#'
# Parameters. ----
save_figs <- TRUE

dir_import <- str_c("data/")
# filename_import_base <- "results"
filename_import_suffix <- "-cleaned"
filename_import_ext <- ".csv"
filepath_import_schools <-
  str_c(dir_import,
        "schools",
        filename_import_suffix,
        filename_import_ext)

filepath_import_persons <-
  str_c(dir_import,
        "persons",
        filename_import_suffix,
        filename_import_ext)


# The save_fig argument with the default set to the global save_figs value
# is used so that save_viz() can be called for all visualizations
# and the appropriate action is taken based on the global value.
save_viz <-
  function(viz,
           filename_save = deparse(substitute(viz)),
           dir_save = "figs/",
           filename_save_ext = ".png",
           filepath_save = str_c(dir_save, filename_save, filename_save_ext),
           w = 11,
           h = 7,
           overwrite = TRUE,
           create_backup = FALSE,
           # save = TRUE) {
           save = save_figs) {

    if(save == TRUE) {

      ggplot2::ggsave(
        viz,
        filename = filepath_save,
        units = "in",
        width = w,
        height = h
      )
      message("Saved ", filename_save, " as ", filepath_save, ".")
    } else {
      message("Not saving ", filename_save, ".")
    }
  }

#'
#'
#'
# Constants. ----
color_neutral_1 <- "black"
color_neutral_2 <- "red"
linetype_neutral_1 <- "solid"
linetype_neutral_2 <- "dashed"
size_neutral_1 <- 2
size_neutral_2 <- 1
width_strip_wrap <- 12
width_xy_wrap <- 10
#'
#'
#'
#'
#+ results = "hide"
# Hide results in this chunk because data is printed out for debugging purposes.
# Import. ----
import_cleanly <- function(filepath) {
  # Note that there are some rows that were not completely cleaned.
  # These rows have NAs in the complvl_num column.
  filepath %>%
    readr::read_csv() %>%
    filter(!is.na(complvl_num))
}

add_baseline_calcs <- function(d) {
  d %>%
    group_by(year, school, city, conf, comp_shortname, comp) %>%
    mutate(advanced = ifelse(
      complvl == "State",
      as.logical(NA),
      ifelse(!is.na(lead(complvl, 1)), TRUE, FALSE)
    )) %>%
    ungroup() %>%
    group_by(year, conf, complvl, complvl_num, comp_shortname, comp) %>%
    mutate(
      cnt_bycomp = n(),
      rank = row_number(desc(score)),
      prank = percent_rank(score)
    ) %>%
    mutate(defeat_cnt = cnt_bycomp - rank) %>%
    mutate(win = ifelse(rank == 1, TRUE, FALSE)) %>%
    ungroup()
}

schools_all <-
  filepath_import_schools %>%
  import_cleanly() %>%
  add_baseline_calcs()

schools_clemens <- schools_all %>% filter(school == "Clemens")
schools_all %>% filter(school == "Clemens") %>% filter(comp_shortname == "sci")

# 9608 rows with too many values if sep is not specified.
# Only 517 if sep is specified (and 2 rows with too few values).
persons_all <-
  filepath_import_persons %>%
  import_cleanly() %>%
  add_baseline_calcs() %>%
  separate(name,
           c("name_last", "name_first"),
           sep = ", ",
           remove = FALSE)

persons_elhabr <- persons_all %>% filter(name_last == "Elhabr")
persons_elhabr

comps_valid <-
  schools_all %>% distinct(comp) %>% arrange(comp) %>% pull(comp)
num_comps <- length(comps_valid)

complvls_valid <-
  schools_all %>% distinct(complvl) %>% arrange(complvl) %>% pull(complvl)
num_complvls <- length(complvls_valid)

#'
#' # Introduction
#'
#' In this project I investigate the results of Texas school UIL Academic competitions.
#'
#' ## Motivation
#'
#' Probably the main reason that I am interested in this subject is
#' because I myself competed in these competitions when I was in school!
#' I would like to know how I stacked up histogrically among all students.
#'
#' Additionaly, aside from standardized testing, these competitions might be the best way
#' of identifiing "strong" sutdents and schools. In fact, given the competitive
#' nature of this domain, one could argue that these competitions provide
#' better gauges of "elite" sutdents and schools. Thus, analysis of competition
#' results is useful for revealing highly reputable schools and spotlighting
#' superior students.
#'
#' Finally, as if I needed any other excuse to invesitgate this data,
#' I saw this data set as a great opportunity to practice my
#' `#rstats` and `#datascience` skills.
#'
#' ## Getting the Data
#'
#' ...
#'
#' ## About the Data
#'
#' First, before doing any kind of analysis, one should be aware of some "meta" details.
#'
#' The UIL categorizes schools into one of six "conferences". The conference labels
#' range from `1A`, `2A`, ..., `6A`, where the increasing leading digit (i.e. `1`, `2`, etc.)
#' generally corresponds to increasing school size. Schools only compete against
#' other schools in their conference.
#'
#' The UIL defines 3 levels of competition (in order of "difficulty"): `District`, `Region`, and `State`.
#' Winning a `District` competitions, results in a `Region` competition appearance,
#' and, subsequently, winning a `Region` competiton results in a `State` competition appearance.
#' (Keep in mind that schools still only compete against other schools in their same
#' conference, even as they advance.)
#' Note that The UIL identifies 32 total `District`s in Texas, which are aggregated into 4 `region`s.
#'
#' For schools, winning is a "winner-take-all" matter: only the school with the most combined points
#' among its individual competitors advances. On ther other hand, an individual
#' may advance even if his school does not win if he places among the top `n`.
#' (Note that $n$is dependent on the competition type. See the UIL rules for more details.).
#'
#'
#' There are 5 different competitions "types" in the data set:
#' `Calculator Applications`, `Computer Science`, `Mathematics`, `Number Sense`, and `Science`.
#' There are actually many more UIL competition types than those analyzed here
#' (including competitons for theatre, band, etc.), but these are the ones
#' for which I retrieved data.
#'
#'
#'
#+ include = FALSE
session::save.session("R/analyze_00.RData")

#'
#'
#'


