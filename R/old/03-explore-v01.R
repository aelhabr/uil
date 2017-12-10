#' ---
#' title: "Exploring Texas school Academic UIL Competition Results"
#' author: "Tony"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    theme: united
#'    highlight: tango
#' ---
#'
#'
#'
#+ global_options, include = FALSE
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

theme_set(theme_minimal())

# library("printr")
#'
#'
#'
# Parameters. ----
save_figs <- FALSE
run_old_code <- FALSE

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


dir_figs <- str_c("figs/")
filename_figs_ext <- ".png"

# The save_fig argument with the default set to the global save_figs value
# is used so that save_viz() can be called for all visualizations
# and the appropriate action is taken based on the global value.
save_viz <-
  function(viz,
           filename_viz = deparse(substitute(viz)),
           # filename_viz = rlang::quo_name(viz)
           w = 7,
           h = 7,
           overwrite = TRUE,
           create_backup = FALSE,
           save = save_figs) {
    if (save == TRUE) {
      # filename_viz <- deparse(substitute(viz))
      filepath_viz <-
        str_c(dir_figs, filename_viz, filename_figs_ext)
      # ggsave(path = filepath_viz, plot = viz, units = "in", width = w, height = h)
      ggsave(
        filename = filepath_viz,
        units = "in",
        width = w,
        height = h
      )
      # temisc::save_as_ext(
      #     viz,
      #     ext = filename_figs_ext,
      #     dirpath = dir_figs,
      #     filename = filename_viz,
      #     # filename = substitute(deparse(viz_comp_score_ranges)),
      #     overwrite = overwrite,
      #     create_backup = create_backup
      # )
      message("Saved ", filename_viz, " as ", filepath_viz, ".")
    } else {
      message("Not saving ", filename_viz, ".")
    }
  }

#'
#'
#'
# Constants. ----
# group_vars_unique_schools_quos <- quos(year, conf, comp_shortname, complvl, comp, complvl_num, school, city)
# group_vars_unique_persons_quos <- c(group_vars_unique_schools_quos, quos(name_last))

compute_summary <- function(grouped_df) {
  grouped_df %>%
    summarise_at(
      vars(score),
      funs(
        mean,
        median,
        sd,
        min,
        max,
        z_n1 = mean(.) - sd(.),
        z_p1 = mean(.) + sd(.),
        q05 = quantile(., 0.05),
        q95 = quantile(., 0.95)
      )
    )
}

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
#' # Analysis
#'
#' With this knowledge in mind, let's take begin with "basic" analysis, and
#' work our way towards nddressing more specific questions.
#'
#'
#' ## "Competition Level" Analysis
#'
#' ### Which _districts, regions, and conferences_ have the most _distinct schools_? [^fn_competition_grouping]
#'
#' [^fn_competition_groupings]:
#' As a technical note, districts, regions, and conferences are not all of the same "type"
#' in the data. Rather,
#' `District` and `Region` are classified as disticnt factors in the `complvl`
#' column (along with `State`) and conferences are
#' classified in their own `conf` column. Nevertheless, for the purpose of exploration,
#' these diferent "competition groupings" each stratify the sample population in some manner.
#'
#+ results = "hide"
rank_and_arrange <-
  function(d,
           colname_cnt_char = "cnt",
           colname_rank_char = "rank") {
    colname_cnt_quo <- rlang::sym(colname_cnt_char)
    colname_rank_quo <- rlang::sym(colname_rank_char)
    d %>%
      mutate(!!colname_rank_quo := row_number(desc(!!colname_cnt_quo))) %>%
      arrange(!!colname_rank_quo)
  }

compute_cnt_bycomplvl <- function(complvls = complvls_valid) {
  schools_all %>%
    filter(complvl %in% complvls) %>%
    group_by(year, complvl_num) %>%
    distinct(year, complvl_num, school) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    group_by(complvl_num) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    rank_and_arrange()
  # mutate(rank = row_number(desc(cnt))) %>%
  # arrange(rank)
}
cnt_bycomplvl_district <- compute_cnt_bycomplvl("District")
cnt_bycomplvl_region <- compute_cnt_bycomplvl("Region")

cnt_bycomplvl_byconf <-
  schools_all %>%
  distinct(year, conf, school) %>%
  group_by(year, conf) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  group_by(conf) %>%
  summarise(cnt = mean(cnt)) %>%
  ungroup() %>%
  rank_and_arrange()
#'
#'
#'
#+ fig.show = "hide"
labs_cnt_byx <-
  labs(x = NULL, y = NULL)
theme_cnt_byx <-
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
visualize_cnt_byx <-
  function(d,
           x_char = "complvl_num",
           subtitle_suffix = "") {
    d %>%
      ggplot(aes_string(x = x_char, y = "cnt")) +
      geom_point(aes(size = desc(rank))) +
      # scale_color_distiller(palette = "Reds") +
      # hrbrthemes::scale_color_ipsum() +
      labs(title = "Count of Schools",
           subtitle = str_c("By ", subtitle_suffix)) +
      labs_cnt_byx +
      theme_cnt_byx
  }

viz_cnt_bycomplvl_district <-
  cnt_bycomplvl_district %>%
  visualize_cnt_byx(x_char = "complvl_num", subtitle_suffix = "District")
viz_cnt_bycomplvl_district

viz_cnt_bycomplvl_region <-
  cnt_bycomplvl_region %>%
  visualize_cnt_byx(x_char = "complvl_num", subtitle_suffix = "Region")
viz_cnt_bycomplvl_region

viz_cnt_bycomplvl_byconf <-
  cnt_bycomplvl_byconf %>%
  visualize_cnt_byx(x_char = "conf", subtitle_suffix = "Conference")
viz_cnt_bycomplvl_byconf
#'
#'
#'
#+ include = FALSE
# {viz_cnt_bycomplvl_district + viz_cnt_bycomplvl_region} / viz_cnt_bycomplvl_byconf + plot_layout(ncol = 1)
#'
#'
#'
library("grid")
library("gridExtra")
grid.arrange(
  viz_cnt_bycomplvl_district,
  viz_cnt_bycomplvl_region + labs(title = NULL),
  viz_cnt_bycomplvl_byconf + labs(title = NULL),
  ncol = 2,
  nrow = 2,
  # top = textGrob("Count of Schools", gp = gpar(fontsize = 14, family = "text", font = 2),
  layout_matrix = rbind(c(1, 1), c(2, 3))
)
#'
#' It seems fair to say that the distribution of schools among districts/regions/conferences
#' is relatively even. This is to be expected since the UIL (presumably)
#' tries to divide schools evenly among each grouping (to the extent possible) in
#' order to stimulate fair competition.
#'
#' ### Which competition types have the most individual competitors and distinct schools?
#'
#+ results = "hide"
compute_cnt_bycomp_byx <- function(d) {
  d %>%
    group_by(year, comp) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    group_by(comp) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    rank_and_arrange()
}
cnt_bycomp_byperson <- persons_all %>% compute_cnt_bycomp_byx()
cnt_bycomp_byschool <- schools_all %>% compute_cnt_bycomp_byx()

#'
#'
#'
#+ fig.show = "hide"
lab_persons <- "Individual Competitors"
lab_schools <- "Distinct Schools"
visualize_cnt_bycomp_byx_base <-
  function(d,
           x_char = "comp",
           color_char = x_char,
           title_suffix = "",
           lab_subtitle = "By Competition Type") {
    d %>%
      ggplot(aes_string(x = x_char, y = "cnt")) +
      geom_point(aes_string(color = color_char), size = 4) +
      # geom_point(aes(color = comp, size = desc(rank))) +
      # scale_radius(range = c(3, 7)) +
      labs(title = str_c("Count of ", title_suffix),
           subtitle = lab_subtitle) +
      labs_cnt_byx +
      theme_cnt_byx
  }


visualize_cnt_bycomp_byx <- function(d, title_suffix = "") {
  d %>%
    visualize_cnt_bycomp_byx_base(x_char = "comp",
                                  title_suffix = title_suffix,
                                  lab_subtitle = "By Competition Type")
}

viz_cnt_bycomp_byperson <-
  cnt_bycomp_byperson %>%
  visualize_cnt_bycomp_byx(title_suffix = lab_persons)
viz_cnt_bycomp_byperson

viz_cnt_bycomp_byschool <-
  cnt_bycomp_byschool %>%
  visualize_cnt_bycomp_byx(title_suffix = lab_schools)
viz_cnt_bycomp_byschool
#'
#'
#'
grid.arrange(
  viz_cnt_bycomp_byperson + labs(subtitle = NULL),
  viz_cnt_bycomp_byschool + labs(subtitle = NULL),
  nrow = 2
)
#'
#' ####  ... at _each competition level_?
#'
#+ results = "hide"
compute_cnt_bycomp_byx_bycomplvl <- function(d) {
  d %>%
    group_by(year, complvl, comp) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    group_by(complvl, comp) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    group_by(complvl) %>%
    mutate(rank = row_number(desc(cnt))) %>%
    ungroup() %>%
    arrange(complvl, rank)
}
cnt_bycomp_byperson_bycomplvl <-
  persons_all %>% compute_cnt_bycomp_byx_bycomplvl()
cnt_bycomp_byschool_bycomplvl <-
  schools_all %>% compute_cnt_bycomp_byx_bycomplvl()
#'
#'
#'
#+ fig.show = "hide"
visualize_cnt_bycomp_byx_bycomplvl <-
  function(d, title_suffix = "") {
    d %>%
      visualize_cnt_bycomp_byx_base(x_char = "comp",
                                    title_suffix = title_suffix,
                                    lab_subtitle = "By Competition Type and Competition Level") +
      facet_grid(complvl ~ .,
                 scales = "free",
                 labeller = label_wrap_gen(width = 12))
  }
viz_cnt_bycomp_byperson_bycomplvl <-
  cnt_bycomp_byperson_bycomplvl %>%
  visualize_cnt_bycomp_byx_bycomplvl(title_suffix = lab_persons)
viz_cnt_bycomp_byperson_bycomplvl

viz_cnt_bycomp_byschool_bycomplvl <-
  cnt_bycomp_byschool_bycomplvl %>%
  visualize_cnt_bycomp_byx_bycomplvl(title_suffix = lab_schools)
viz_cnt_bycomp_byschool_bycomplvl
#'
#'
#'
grid.arrange(
  viz_cnt_bycomp_byperson_bycomplvl + labs(subtitle = NULL),
  viz_cnt_bycomp_byschool_bycomplvl + labs(subtitle = NULL),
  nrow = 2
)
#'
#' #### ... in _each conference_?
#'
#+ results = "hide"
compute_cnt_bycomp_byx_bycomplvl_byconf <- function(d) {
  d %>%
    group_by(year, complvl, comp, conf) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    group_by(complvl, comp, conf) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    group_by(complvl, comp) %>%
    mutate(rank = row_number(desc(cnt))) %>%
    ungroup() %>%
    arrange(comp, complvl, rank)
}

cnt_bycomp_byperson_bycomplvl_byconf <-
  persons_all %>%
  compute_cnt_bycomp_byx_bycomplvl_byconf()
cnt_bycomp_byschool_bycomplvl_byconf <-
  schools_all %>%
  compute_cnt_bycomp_byx_bycomplvl_byconf()

#'
#'
#'
visualize_cnt_bycomp_byx_bycomplvl_byconf <-
  function(d, title_suffix = "") {
    d %>%
      visualize_cnt_bycomp_byx_base(
        x_char = "conf",
        color_char = "comp",
        title_suffix = title_suffix,
        lab_subtitle = "By Competition Type, Competition Level, and Conference"
      ) +
      facet_grid(complvl ~ comp,
                 scales = "free",
                 labeller = label_wrap_gen(width = 12))
    # facet_grid(comp ~ complvl, scales = "free", labeller = label_wrap_gen(width = 12))
  }
viz_cnt_bycomp_byperson_bycomplvl_byconf <-
  cnt_bycomp_byperson_bycomplvl_byconf %>%
  visualize_cnt_bycomp_byx_bycomplvl_byconf(title_suffix = lab_persons)
viz_cnt_bycomp_byperson_bycomplvl_byconf

viz_cnt_bycomp_byschool_bycomplvl_byconf <-
  cnt_bycomp_byschool_bycomplvl_byconf %>%
  visualize_cnt_bycomp_byx_bycomplvl_byconf(title_suffix = lab_schools)
viz_cnt_bycomp_byschool_bycomplvl_byconf

#'
#' Just by inspection, science appears to be the answer to the numerous
#' variations of the "Which competition type/level has the most ...?" question.
#'
#' ### Which competition levels have the most distinct schools in _each conference_?
#'
#+ results = "hide"
aggregate_cnt_bycomp_byx_bycomplvl_byconf <- function(d) {
  d %>%
    group_by(complvl, conf) %>%
    # summarise(rank = mean(rank)) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    group_by(complvl) %>%
    mutate(rank = row_number(desc(cnt))) %>%
    arrange(complvl, rank)
}
# cnt_byperson_bycomplvl_byconf <-
#   cnt_bycomp_byperson_bycomplvl_byconf %>%
#   aggregate_cnt_bycomp_byx_bycomplvl_byconf()

cnt_byschool_bycomplvl_byconf <-
  cnt_bycomp_byschool_bycomplvl_byconf %>%
  aggregate_cnt_bycomp_byx_bycomplvl_byconf()
#'
#'
#'
visualize_cnt_byx_bycomplvl_byconf <-
  function(d, title_suffix = "") {
    d %>%
      visualize_cnt_bycomp_byx_base(
        x_char = "conf",
        color_char = NULL,
        title_suffix = title_suffix,
        lab_subtitle = "By Competition Level and Conference"
      ) +
      facet_grid(. ~ complvl,
                 scales = "free",
                 labeller = label_wrap_gen(width = 12))
  }
# viz_cnt_byperson_bycomplvl_byconf <-
#   cnt_byperson_bycomplvl_byconf %>%
#   visualize_cnt_byx_bycomplvl_byconf(title_suffix = lab_persons)
# viz_cnt_byperson_bycomplvl_byconf

viz_cnt_byschool_bycomplvl_byconf <-
  cnt_byschool_bycomplvl_byconf %>%
  visualize_cnt_byx_bycomplvl_byconf(title_suffix = lab_schools)
viz_cnt_byschool_bycomplvl_byconf
#'
#' Note the difference in this "Which has the most ...?" question--here, we
#' are aggregating over different competition types, whereas previously we had
#' distinguished between each).
#'
#+ include = FALSE
# # Attempt to make a NSE version that combines the functionality of the previous two functions.
# compute_cnt_bycomp_byx_nse <- function(d, complvls = complvls_valid, ...){
#   group_1_vars_quos <- rlang::quos(...)
#   # group_1_vars_quos <- quos(group_1_vars_quos)
#   # group_1_vars_quos <- rlang::quos(year, complvl, comp_shortname, comp, conf)
#
#   group_2_vars_quos <- setdiff(rlang::quo_name(group_1_vars_quos), "year")
#   # group_2_vars_quos <- rlang::quos(complvl, comp_shortname, comp, conf)
#   group_3_vars_quos <- rlang::quos(complvl, conf)
#   d %>%
#     filter(complvl %in% complvls) %>%
#     group_by(!!!group_1_vars_quos) %>%
#     summarise(cnt = n()) %>%
#     ungroup() %>%
#     group_by(!!!group_2_vars_quos) %>%
#     summarise(cnt = mean(cnt)) %>%
#     ungroup() %>%
#     group_by(!!!group_3_vars_quos) %>%
#     mutate(rank = row_number(desc(cnt))) %>%
#     arrange(rank)
# }

#'
#'
#'
#+ include = FALSE
# analysis_comp_bookmark ----

#'
#' ## "Competition Type" Analysis
#'
#' ### What are the score distributions for each _competition type_?
#'
#+ results = "hide"
comp_stats_byperson <-
  persons_all %>%
  group_by(comp) %>%
  compute_summary() %>%
  ungroup() %>%
  arrange(comp)
comp_stats_byperson

#'
#'
#'
num_comps <- length(comps_valid)
q05_min <- min(comp_stats_byperson$q05)
q95_max <- max(comp_stats_byperson$q95)


# Must join with separate data frame in order to plot mean, etc. properly with facets.
# Must specify only one value for mean, etc. (because metrics are caclualted for each year,
# and the histogram does not differentiate among years).
# Use scale_color_hue() (instead of scale_fill_manual() because values argument does not need to be specified.
lab_comp <- "Distribution of Scores for Each Competition Type"
labs_comp_stats <- labs(x = NULL,
                 y = NULL,
                 title = lab_comp)
theme_comp_stats <-
  theme(legend.position = "none")

color_neutral_1 <- "black"
color_neutral_2 <- color_neutral_2

add_common_viz_comp_stats_elements <- function(viz) {
  viz +
    guides(fill = FALSE) +
    scale_x_continuous(limits = c(q05_min, q95_max)) +
    scale_y_continuous(labeller = scales::pretty_breaks()) +
    scale_y_continuuos(labeller = function(x) round(x, 2)) +
    # facet_wrap( ~ comp, scales = "free") +
    facet_wrap(
      ~ comp,
      scales = "free",
      nrow = num_comps,
      labeller = label_wrap_gen(width = 12),
      strip.position = "right"
    ) +
    labs_comp_stats +
    theme_comp_stats
}

viz_comp_stats <-
  persons_all %>%
  inner_join(comp_stats_byperson) %>%
  ggplot(aes(x = score)) +
  # geom_histogram(aes(x = score, fill = comp)) +
  geom_density(aes(fill = comp)) +
  add_common_viz_comp_stats_elements() +
  geom_vline(
    aes(xintercept = mean),
    color = color_neutral_1,
    linetype = "dashed",
    size = 2
  ) +
  geom_vline(aes(xintercept = z_n1), color = color_neutral_1, size = 2) +
  geom_vline(aes(xintercept = z_p1), color = color_neutral_1, size = 2) +
  labs(subtitle = "Aggregated over Year, Conference, and Competition Level",
       caption = "mean = dashed line; z = -1 and z = 1 threshholds = solid lines") +
  theme(
    strip.text = element_blank(),
    axis.text.y = NULL,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs_comp_stats +
  theme_comp_stats
viz_comp_stats
# save_viz(viz_comp_stat, w = 7, h = 11)

#'
#' ### What are the score distributions for each competition type ...
#'
#' #### _by year_?
#'
#+ results = "hide"
comp_stats_byyear <-
  persons_all %>%
  group_by(comp, year) %>%
  compute_summary() %>%
  ungroup() %>%
  arrange(comp, year)
comp_stats_byyear
#'
#'
#'
q05_min <- min(comp_stats_byyear$q05)
q95_max <- max(comp_stats_byyear$q95)

year_min <- min(comp_stats_byyear$year)
year_max <- max(comp_stats_byyear$year)
years_labs <- seq(year_min, year_max, by = 4)


viz_comp_stats_byear <-
  comp_stats_byyear %>%
  ggplot() +
  geom_segment(
    aes(
      x = year,
      y = q05,
      xend = year,
      yend = q95,
      color = comp
    ),
    linetype = "dotted",
    size = 1
  ) +
  geom_segment(aes(
    x = year,
    y = z_n1,
    xend = year,
    yend = z_p1,
    color = comp
  ), size = 2) +
  geom_point(aes(x = year, y = mean), fill = "black", size = 2) +
  scale_x_continuous(breaks = years_labs) +
  scale_y_continuous(limits = c(q05_min, q95_max)) +
  # facet_wrap( ~ comp_shortname, scales = "free") +
  facet_wrap(
    ~ comp,
    scales = "free",
    nrow = num_comps,
    labeller = label_wrap_gen(width = 12),
    strip.position = "right"
  ) +
  coord_flip() +
  labs(subtitle = "By Year") +
  labs_comp_stats +
  theme_comp_stats
viz_comp_stats_byear
# save_viz(viz_comp_stats_byear, w = 7, h = 11)

#'
#' #### by  _competition level_?
#'
#+ results = "hide"

comp_stats_bycomplvl <-
  persons_all %>%
  group_by(comp, complvl) %>%
  compute_summary() %>%
  ungroup()
comp_stats_bycomplvl
#'
#'
#'
viz_comp_stats_bycomplvl <-
  comp_stats_bycomplvl %>%
  ggplot(aes(
    x = complvl,
    y = mean,
    ymin = z_n1,
    ymax = z_p1
  )) +
  geom_pointrange(aes(color = comp)) +
  # scale_y_continuous(limits = c(q05_min, q95_max)) +
  # facet_wrap( ~ comp_shortname, scales = "free") +
  facet_wrap(
    ~ comp,
    scales = "free",
    nrow = num_comps,
    labeller = label_wrap_gen(width = 12),
    strip.position = "right"
  ) +
  coord_flip() +
  labs(subtitle = "By Competition Level") +
  labs_comp_stats +
  theme_comp_stats
viz_comp_stats_bycomplvl
# save_viz(viz_comp_stats_bycomplvl, w = 7, h = 11)
#'
#' ### Which _competition types_ have the largest increase in scores with increasing _competition level_?
#'
#+ results = "hide"
unitize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

comp_stats_bycomplvl_scaled <-
  persons_all %>%
  group_by(comp, complvl) %>%
  compute_summary() %>%
  ungroup() %>%
  # group_by(complvl) %>%
  mutate_at(vars(mean, sd, min, max), funs(scaled = unitize))
comp_stats_bycomplvl_scaled

comp_stats_bycomplvl_diffs <-
  persons_all %>%
  group_by(comp, complvl) %>%
  compute_summary() %>%
  ungroup() %>%
  group_by(comp) %>%
  # mutate_at(vars(mean, median, sd), funs(diff_pct = diff(c(NA, .)) / .))
  mutate_at(vars(mean, sd), funs(
    diff1_pct = (diff(c(NA, .), lag = 1) / lag(., 1)),
    diff2_pct = (diff(c(NA, NA, .), lag = 2) / lag(., 2))
  )) %>%
  mutate_at(vars(complvl),
            funs(
              complvl_diff1 = str_c(lag(.), " to ", .),
              complvl_diff2 = str_c(lag(., 2), " to ", .)
            ))
comp_stats_bycomplvl_diffs %>%
  select(
    comp,
    complvl,
    mean,
    median,
    sd,
    complvl_diff1,
    mean_diff1_pct,
    complvl_diff2,
    mean_diff2_pct
  )

#'
#'
#'
lab_comp_diffpct <-
  "% Increase in Score Distributions for Each Competition Type"
labs_stats_byx_diffs <- labs(x = NULL,
                  y = NULL,
                  title = lab_comp_diffpct)
theme_comp_stats_byx_diffs <-
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
viz_comp_stats_bycomplvl_diffs <-
  comp_stats_bycomplvl_diffs %>%
  filter(!is.na(mean_diff1_pct)) %>%
  ggplot(aes(x = comp, y = mean_diff1_pct)) +
  geom_col(aes(
    group = complvl_diff1,
    fill = comp,
    alpha = complvl
  )) +
  guides(fill = FALSE, size = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(subtitle = "By Competition Level") +
  labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_bycomplvl_diffs
#'
#'
#'
#= results = "hide"
comp_stats_bycomplvl_diffs_tidy <-
  comp_stats_bycomplvl_diffs %>%
  filter(!is.na(mean_diff1_pct) | !is.na(mean_diff2_pct)) %>%
  gather(complvl_diff,
         complvl_diff_label,
         c("complvl_diff1", "complvl_diff2")) %>%
  filter(!is.na(complvl_diff_label)) %>%
  gather(metric, value, c("mean_diff1_pct", "mean_diff2_pct")) %>%
  filter(!is.na(value)) %>%
  filter(!(complvl_diff == "complvl_diff1" &
             metric == "mean_diff2_pct")) %>%
  filter(!(complvl_diff == "complvl_diff2" &
             metric == "mean_diff1_pct")) %>%
  ungroup() %>%
  arrange(comp)
#'
#'
#'
viz_comp_stats_bycomplvl_diffs_2 <-
  comp_stats_bycomplvl_diffs_tidy %>%
  mutate_at(
    vars(complvl_diff_label),
    funs(factor),
    levels = c("District to Region", "Region to State", "District to State")
  ) %>%
  ggplot(aes(x = complvl_diff_label, y = value)) +
  geom_col(aes(fill = comp, alpha = complvl_diff_label)) +
  guides(fill = FALSE, size = FALSE) +
  # geom_smooth(method = "loess", se = FALSE, color = color_neutral_2, size = 2) +
  # geom_hline(aes(yintercept = 1), size = 2, fill = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_grid( ~ comp, scales = "free", labeller = label_wrap_gen(width = 12)) +
  labs(subtitle = "By Competition Level") +
  theme(axis.text.x = element_blank()) +
  labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_bycomplvl_diffs_2

#'
#' ### How have the score ranges differences for each comptition type at each competition level changed by year?
#'
#+ results = "hide"
comp_stats_byyear_diffs <-
  persons_all %>%
  group_by(year, comp) %>%
  compute_summary() %>%
  ungroup() %>%
  arrange(comp, year) %>%
  mutate_at(vars(mean, median, sd),
            funs(diff_pct = diff(c(NA, .)) / .))
comp_stats_byyear_diffs
#'
#'
#'
viz_comp_stats_byyear_diffs <-
  comp_stats_byyear_diffs %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line(aes(color = comp), size = 2) +
  scale_x_continuous(breaks = years_labs) +
  guides(color = FALSE) +
  # geom_smooth(se = FALSE, color = "black", size = 1) +
  facet_grid(comp ~ ., scales = "free", labeller = label_wrap_gen(width = 12)) +
  labs(subtitle = "By Year") +
labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_byyear_diffs
#'
#'
#' ## "Very Specific" Questions
#'
#' ### Who competed in the most competitions?
#'
#'
comp_stats_byperson <-
  persons_all %>%
  group_by(name, school) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  arrange(desc(cnt))
comp_stats_byperson

#'
#' ### From which _conferences_ do the individuals who competed the most come?
#'
num_top <- 100
comps_stats_byperson_top <-
  comp_stats_byperson %>%
  slice(1:num_top)

comps_stats_byperson_top %>%
  inner_join(persons_all) %>%
  group_by(conf) %>%
  distinct(name, school) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(rank = row_number(desc(cnt))) %>%
  arrange(rank)
#'
#' ### To what extent have individual competitors "carried" their teams?
#'
#+ results = "hide"
# Apply filter and deselect comp for debugging.
comp_stats_byperson_byschool <-
  persons_all %>%
  # filter(year == 2016) %>% # filter(conf == "1A") %>% filter(complvl == "state") %>% filter(comp_shortname == "num") %>%
  group_by(comp_shortname, complvl, conf, year, school) %>%
  mutate(within_group_cnt = n()) %>%
  filter(within_group_cnt > 3) %>%
  mutate(within_group_rank = row_number(desc(score))) %>%
  filter(within_group_rank %in% c(1, 2)) %>%
  arrange(desc(within_group_rank)) %>%
  mutate(within_group_diff = score - lag(score)) %>%
  filter(within_group_rank == 1) %>%
  select(-within_group_rank) %>%
  ungroup() %>%
  mutate(group_score = score - within_group_diff) %>%
  mutate(among_group_rank = row_number(desc(within_group_diff))) %>%
  # select(-comp) %>%
  arrange(among_group_rank)
comp_stats_byperson_byschool %>%
  mutate(within_group_diff_prank = percent_rank(within_group_diff)) %>%
  # mutate(within_group_diff_top10pct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(within_group_diff_top10pct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  select(
    among_group_rank,
    within_group_diff,
    within_group_diff_prank,
    within_group_diff_top10pct,
    score,
    group_score,
    everything()
  )

comp_stats_byperson_byschool %>%
  group_by(school) %>%
  summarise(among_group_rank = mean(among_group_rank)) %>%
  ungroup() %>%
  arrange(among_group_rank)

#'
#'
#'
viz_comp_stats_byperson_byschool <-
  comp_stats_byperson_byschool %>%
  mutate(within_group_diff_prank = percent_rank(within_group_diff)) %>%
  # mutate(within_group_diff_top10pct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(within_group_diff_top10pct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  # slice(1:100) %>%
  sample_frac(size = 0.01) %>%
  ggplot(aes(x = score, y = group_score)) +
  geom_point(aes(size = within_group_diff, color = within_group_diff_top10pct)) +
  scale_color_manual(values = c("black", color_neutral_2)) +
  geom_abline(
    aes(intercept = 0, slope = 1),
    color = "black",
    linetype = "dashed",
    size = 2
  ) +
  geom_abline(aes(
    intercept = -quantile(within_group_diff, 0.9),
    slope = 1
  ),
  color = color_neutral_2,
  size = 2) +
  labs(
    title = "Individual Score vs. Score of Team",
    caption = str_c(
      "Outside of solid red line generally corresponds to top 10% \"carrying\"."
    ),
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "none")
viz_comp_stats_byperson_byschool

#'
#' ### Which siblings competed in the most competitions?
#'
#+ results = "hide"
siblings_0 <-
  persons_all %>%
  group_by(year,
           conf,
           comp,
           complvl,
           comp,
           complvl_num,
           school,
           city,
           name_last) %>%
  mutate(rank_max = rank(name_last, ties.method = "max")) %>%
  ungroup()
siblings_0 %>% filter(rank_max > 1)
siblings_0 %>% filter(school == "Clemens") %>% filter(rank_max > 1)

siblings <-
  siblings_0 %>%
  filter(rank_max > 1) %>%
  left_join(
    siblings_0 %>%
      # rename_at(vars(name, name_first, score, advanced), funs(str_c(., "_sibling"))) %>%
      filter(rank_max > 1),
    by = c(
      "name_last",
      "school",
      "city",
      "complvl_num",
      "year",
      "conf",
      "complvl",
      "comp",
      "comp"
    ),
    suffix = c("", "_sibling")
  ) %>%
  filter(name_first != name_first_sibling) %>%
  mutate(name_first_pair = str_c(name_first, " & ", name_first_sibling)) %>%
  ungroup() %>%
  select(name, name_first, name_last, name_first_sibling, everything()) %>%
  # distinct(year, school, city, complvl, complvl_num, conf, comp, name_last, .keep_all = TRUE) %>%
  arrange(name_last, name_first, school) %>%
  select(name_first_pair, everything())
siblings %>% filter(school == "Clemens")

#'
#'
#'
siblings_cnt <-
  siblings %>%
  group_by(name_last, name_first_pair) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  distinct(name_last, cnt, .keep_all = TRUE) %>%
  mutate(rank = row_number(desc(cnt))) %>%
  arrange(rank)
siblings_cnt

siblings_cnt_elhabr <-
  siblings_cnt %>%
  filter(name_last == "Elhabr")

#'
#' I was dissapointed to find out that my brother and I were not at
#' the very top of this list. Nevertheless, we did well--we
#' ranked `r siblings_cnt_elhabr$rank`, having competed in
#' `r siblings_cnt_elhabr$cnt` competitions together.
#'
#' ### Which siblings "defeated" the most people?
#'
colnames_select <-
  names(siblings) %>%
  str_subset("^name|^defeat")
colnames_gather <-
  names(siblings) %>%
  str_subset("^defeat")

siblings_defeat_cnt <-
  siblings %>%
  select(one_of(colnames_select)) %>%
  gather(defeat_cnt_type, value, colnames_gather) %>%
  group_by(name_last, name_first_pair) %>%
  summarise(cnt = sum(value)) %>%
  ungroup() %>%
  distinct(name_last, cnt, .keep_all = TRUE) %>%
  mutate(rank = row_number(desc(cnt))) %>%
  arrange(rank)
siblings_defeat_cnt

siblings_defeat_cnt_elhabr <-
  siblings_defeat_cnt %>%
  filter(name_last == "Elhabr")

#'
#' It looks like these rankings are fairly similar. My brother and I
#' are ranked `r siblings_defeat_cnt_elhabr$rank`, having "defeated"
#' `r siblings_defeat_cnt_elhabr$cnt` competitors combined.
#'
#' ### How have people at my school performed?
#'
comp_rank_byschool_byperson <-
  persons_all %>%
  group_by(school, name) %>%
  summarise_at(vars(prank, defeat_cnt), funs(cnt = n(), sum, mean)) %>%
  select(-prank_cnt) %>%
  rename(num_comps = defeat_cnt_cnt) %>%
  mutate(rank = row_number(desc(prank_sum))) %>%
  ungroup()

comp_rank_byschool_byperson %>%
  filter(school == "Clemens") %>%
  select(rank, -school, everything()) %>%
  arrange(rank)

#'
#' Ranking here by cumulative percentile rank (`prank_sum`),
#' it looks like my brother and I are among the top performers
#' in our school's history.
#'
#' ### How has my school performed over time?
#'
comp_rank_byschool_byyear <-
  schools_all %>%
  mutate(state_cnt = ifelse(complvl == "regional" &
                              advanced == TRUE, TRUE, FALSE)) %>%
  group_by(school, year) %>%
  # summarise_at(vars(prank, defeat_cnt, win, advanced), funs(cnt = n(), sum, mean)) %>%
  summarise(
    prank_sum = sum(prank),
    prank_mean = mean(prank),
    defeat_cnt = sum(defeat_cnt),
    advanced_cnt = sum(advanced),
    state_cnt = sum(state_cnt)
  ) %>%
  mutate(rank = row_number(desc(prank_sum))) %>%
  ungroup()

comp_rank_byschool_byyear %>%
  filter(school == "Clemens") %>%
  select(rank, everything()) %>%
  arrange(rank)

#'
#' Judging here again by `prank_sum`, it doesn't look like my
#' school has any consistent trend in performance by year.
#' In some years my school did well, and in others... not so much. In fact,
#' my school has never made an appearnce in a `State` competitions?
#'
#'
#' ### Which schools have "defeated" the most schools in head-to-head competition?
#'
comp_rank_byschool <-
  schools_all %>%
  mutate(state_cnt = ifelse(complvl == "regional" &
                              advanced == TRUE, TRUE, FALSE)) %>%
  group_by(school) %>%
  # summarise_at(vars(prank, defeat_cnt, win, advanced), funs(cnt = n(), sum, mean)) %>%
  summarise(
    prank_sum = sum(prank),
    prank_mean = mean(prank),
    defeat_cnt = sum(defeat_cnt),
    advanced_cnt = sum(advanced),
    state_cnt = sum(state_cnt)
  ) %>%
  # mutate(rank = row_number(desc(prank_sum))) %>%
  mutate_if(is.numeric, funs(rank = row_number(desc(.)))) %>%
  mutate(rank = defeat_cnt_rank) %>%
  ungroup()

comp_rank_byschool %>%
  select(rank, everything()) %>%
  arrange(rank)

comp_rank_byschool_clemens <-
  comp_rank_byschool %>% filter(school == "Clemens")

#'
#' Ranking here by `defeat_cnt` (which corresponds fairly closely to ranks
#' for other metrics gauging success), my school does not appear among the top.
#' My school is ranked `r comp_rank_byschool_clemens$rank` by this metric.
#'
#' # Conclusion
#'
#' ...'=
#'

#'
