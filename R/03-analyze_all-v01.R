
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
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# options(scipen = 1)
options(digits = 2)

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
# library("printr")
library("knitr")
library("kableExtra")
library("formattable")
options(knitr.table.format = "html")

theme_set(theme_minimal())

# library("printr")
#'
#'
#'
# Parameters. ----
save_figs <- FALSE

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
#'
#+ include = FALSE
# Analyze. ----

#'
#'
#' # Analysis
#'
#' With this knowledge in mind, let's take begin with "basic" analysis, and
#' work our way towards nddressing more specific questions.
#'
#'
#+ include = FALSE
# analysis_complvl ----

#'
#' ## "Competition Level" Analysis
#'
#' ### Which _districts, regions, and conferences_ have the most _distinct schools_? [^fn_competition_groupings]
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
           colname_val_char = "cnt",
           colname_rank_char = "rank") {
    colname_val_quo <- rlang::sym(colname_val_char)
    colname_rank_quo <- rlang::sym(colname_rank_char)
    d %>%
      mutate(!!colname_rank_quo := row_number(desc(!!colname_val_quo))) %>%
      arrange(!!colname_rank_quo) %>%
      select(!!colname_rank_quo, everything())
  }

# rank_and_arrange_nse <-
#   function(d,
#            colname_val_bare,
#            colname_rank_bare) {
#     colname_val_quo <- rlang::enquo(colname_val_bare)
#     colname_rank_quo <- rlang::enquo(colname_rank_bare)
#     d %>%
#       mutate(!!colname_rank_quo := row_number(desc(!!colname_val_quo))) %>%
#       arrange(!!colname_rank_quo) %>%
#       select(!!colname_rank_quo, everything())
#   }


compute_cnt_bycomplvl <- function(d, complvls = complvls_valid) {
  d %>%
    filter(complvl %in% complvls) %>%
    group_by(year, complvl_num) %>%
    distinct(year, complvl_num, school) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    group_by(complvl_num) %>%
    summarise(cnt = mean(cnt)) %>%
    ungroup() %>%
    mutate(cnt_mean = mean(cnt)) %>%
    rank_and_arrange()
  # rank_and_arrange_nse(cnt, rank)
}
cnt_bycomplvl_district <- schools_all %>% compute_cnt_bycomplvl("District")
cnt_bycomplvl_region <- schools_all %>% compute_cnt_bycomplvl("Region")

cnt_bycomplvl_byconf <-
  schools_all %>%
  distinct(year, conf, school) %>%
  group_by(year, conf) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  group_by(conf) %>%
  summarise(cnt = mean(cnt)) %>%
  ungroup() %>%
  mutate(cnt_mean = mean(cnt)) %>%
  rank_and_arrange()
#'
#'
#'
#+ fig.show = "hide"
labs_cnt_byx <-
  labs(x = NULL, y = NULL)
theme_cnt_byx <-
  theme(# panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

visualize_cnt_byx <-
  function(d,
           x_char = "complvl_num",
           subtitle_suffix = "") {
    d %>%
      ggplot(aes_string(x = x_char, y = "cnt")) +
      # Coluld make color an optional input.
      geom_point(aes(size = desc(rank)), color = "black") +
      geom_segment(aes_string(xend = x_char, yend = "0"), color = "black") +
      geom_hline(
        aes(yintercept = cnt_mean),
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      # This is for plotting purposes only.
      # mutate(cnt_mean = mean(cnt)) %>%
      # scale_color_distiller(palette = "Reds") +
      # hrbrthemes::scale_color_ipsum() +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
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
# bookmark ----
viz_cnt_bycomplvl_grid <-
  arrangeGrob(
    viz_cnt_bycomplvl_district,
    viz_cnt_bycomplvl_region + labs(title = NULL),
    viz_cnt_bycomplvl_byconf + labs(title = NULL),
    ncol = 2,
    nrow = 2,
    # top = textGrob("Count of Schools", gp = gpar(fontsize = 14, family = "text", font = 2),
    layout_matrix = rbind(c(1, 1), c(2, 3))
  )
grid.arrange(viz_cnt_bycomplvl_grid)
save_viz(viz_cnt_bycomplvl_grid)

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
    # This is for plotting purposes only.
    mutate(cnt_mean = mean(cnt)) %>%
    rank_and_arrange()
}
cnt_bycomp_byperson <- persons_all %>% compute_cnt_bycomp_byx()
cnt_bycomp_byschool <- schools_all %>% compute_cnt_bycomp_byx()

#'
#'
#'
#+ fig.show = "hide"
lab_persons <- "Competitors"
lab_schools <- "Schools"
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
      geom_segment(aes_string(
        xend = x_char,
        yend = "0",
        color = color_char
      )) +
      geom_hline(
        aes(yintercept = cnt_mean),
        color = "black",
        linetype = "dashed",
        size = 1
      ) +
      scale_x_discrete(labels = scales::wrap_format(10)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
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
viz_cnt_comp_byx_grid <-
  arrangeGrob(
    viz_cnt_bycomp_byperson + labs(subtitle = NULL),
    viz_cnt_bycomp_byschool + labs(subtitle = NULL),
    nrow = 2
  )
grid.arrange(viz_cnt_comp_byx_grid)
save_viz(viz_cnt_comp_byx_grid)
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
    # This is for plotting purposes only.
    mutate(cnt_mean = mean(cnt)) %>%
    rank_and_arrange("cnt") %>%
    arrange(complvl, rank)
}
cnt_bycomp_byperson_bycomplvl <-
  persons_all %>% compute_cnt_bycomp_byx_bycomplvl()
cnt_bycomp_byschool_bycomplvl <-
  schools_all %>% compute_cnt_bycomp_byx_bycomplvl()
#'
#'
#'
cnt_bycomp_byperson_bycomplvl <-
  cnt_bycomp_byperson_bycomplvl %>%
  group_by(complvl) %>%
  mutate(cnt_mean = mean(cnt)) %>%
  ungroup()

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
                 labeller = label_wrap_gen(width = 10))
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
viz_cnt_bycomp_byx_bycomplvl_grid <-
  arrangeGrob(
    viz_cnt_bycomp_byperson_bycomplvl + labs(subtitle = NULL) + theme(axis.text.x = element_text(angle = 90)),
    viz_cnt_bycomp_byschool_bycomplvl + labs(subtitle = NULL) + theme(axis.text.x = element_text(angle = 90)),
    ncol = 2
  )
grid.arrange(viz_cnt_bycomp_byx_bycomplvl_grid)
save_viz(viz_cnt_bycomp_byx_bycomplvl_grid)
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
    rank_and_arrange("cnt") %>%
    ungroup() %>%
    group_by(complvl) %>%
    mutate(cnt_mean = mean(cnt)) %>%
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
                 labeller = label_wrap_gen(width = 10))
    # facet_grid(comp ~ complvl, scales = "free", labeller = label_wrap_gen(width = 10))
  }
viz_cnt_bycomp_byperson_bycomplvl_byconf <-
  cnt_bycomp_byperson_bycomplvl_byconf %>%
  visualize_cnt_bycomp_byx_bycomplvl_byconf(title_suffix = lab_persons)
viz_cnt_bycomp_byperson_bycomplvl_byconf
save_viz(viz_cnt_bycomp_byperson_bycomplvl_byconf)

viz_cnt_bycomp_byschool_bycomplvl_byconf <-
  cnt_bycomp_byschool_bycomplvl_byconf %>%
  visualize_cnt_bycomp_byx_bycomplvl_byconf(title_suffix = lab_schools)
viz_cnt_bycomp_byschool_bycomplvl_byconf
save_viz(viz_cnt_bycomp_byschool_bycomplvl_byconf)

#'
#' Just by inspection, science appears to be the answer to the numerous
#' variations of the "Which competition type/level has the most ...?" question.
#'
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
# analysis_comp ----

#'
#' ## "Competition Type" Analysis
#'
#' ### What are the score distributions for each competition type?
#'
#+ results = "hide"
compute_summary <- function(d) {
  d %>%
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
labs_comp_stats <-
  labs(x = NULL,
       y = NULL,
       title = lab_comp)
theme_comp_stats <-
  theme(legend.position = "none")

viz_comp_stats <-
  persons_all %>%
  inner_join(comp_stats_byperson) %>%
  ggplot(aes(x = score)) +
  geom_histogram(aes(fill = comp)) +
  # geom_density(aes(fill = comp)) +
  # add_common_viz_comp_stats_elements() +
  geom_vline(
    aes(xintercept = mean),
    color = "black",
    linetype = "sold",
    size = 2
  ) +
  geom_vline(aes(xintercept = z_n1), color = "black", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = z_p1), color = "black", linetype = "dashed", size = 1) +
  guides(fill = FALSE) +
  scale_x_continuous(limits = c(q05_min, q95_max)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # scale_y_continuuos(breaks = function(x) round(x, 2)) +
  facet_wrap(
    ~ comp,
    scales = "free",
    nrow = num_comps,
    labeller = label_wrap_gen(width = 12),
    strip.position = "right"
  ) +
  # labs(caption = "mean = dashed line; z = -1 and z = 1 threshholds = solid lines") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs_comp_stats +
  theme_comp_stats

viz_comp_stats
save_viz(viz_comp_stats)

#'
#' ### What are the score distributions for each competition type _by year_ and _by competition level_?
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
#+ fig.show = "hide"
# q05_min <- min(comp_stats_byyear$q05)
# q95_max <- max(comp_stats_byyear$q95)
#
year_min <- min(comp_stats_byyear$year)
year_max <- max(comp_stats_byyear$year)
years_labs <- seq(year_min, year_max, by = 4)

visualize_comp_stats_byx <- function(d, x_char = "", subtitle_suffix = "") {
  d %>%
    ggplot(
      aes_string(
        x = x_char,
        y = "mean"
      )
    ) +
    geom_pointrange(aes(ymin = z_n1, ymax = z_p1, color = comp)) +
    # scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(breaks = years_labs) +
    guides(color = FALSE) +
    # Using facet_wrap() so strip.position can be specified.
    facet_wrap(
      ~ comp,
      scales = "free",
      nrow = num_comps,
      labeller = label_wrap_gen(width = 12),
      strip.position = "right"
    ) +
    # coord_cartesian(limits = c(min(min), max(max))) +
    coord_flip() +
    labs(subtitle = str_c("By ", subtitle_suffix)) +
    labs_comp_stats +
    theme_comp_stats
}
viz_comp_stats_byyear <-
  comp_stats_byyear %>%
  visualize_comp_stats_byx(x_char = "year", subtitle_suffix = "Year")
viz_comp_stats_byyear

#'
#'
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
# fig.keep = "hide"
viz_comp_stats_bycomplvl <-
  comp_stats_bycomplvl %>%
  visualize_comp_stats_byx(x_char = "complvl", subtitle_suffix = "Competition Level")
viz_comp_stats_bycomplvl
#'
#'
#'
viz_comp_stats_byx_grid <-
  arrangeGrob(
    viz_comp_stats_byyear,
    viz_comp_stats_bycomplvl + labs(title = ""),
    ncol = 2
  )
grid.arrange(viz_comp_stats_byx_grid)
save_viz(viz_comp_stats_byx_grid)
#'
#' It appears that there is no discernable trends among score distributions
#' across years. Nonetheless, it is evident that score range distributions
#' shift upwards with increasing competition level, as would be expected with
#' more "superior" competitors advancing to higher levels of competition.
#'
#' The lack of pattern in score distributions
#' over time is a strong indication that the tests have about the same difficulty every year.
#' This implies that it would not be unreasonable to compare raw test scores
#' for a given competition type when comparing individuals
#' and schools across different years. (Nonetheless, I account for year-t0-year
#' variation in my calculations.)
#'
#' ### Which competition types have the largest increase in scores with increasing competition level?
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
    mean_diff2_pct,
    everything()
  )

#'
#'
#'
#+ fig.show = "hide"
lab_comp_diffpct <-
  "% Change in Score Distributions for Each Competition Type"
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
  scale_alpha_discrete(range = c(0.2, 1)) +
  guides(fill = FALSE, size = FALSE) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
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
#+ fig.show = "hide"
viz_comp_stats_bycomplvl_diffs_2 <-
  comp_stats_bycomplvl_diffs_tidy %>%
  mutate_at(
    vars(complvl_diff_label),
    funs(factor),
    levels = c("District to Region", "Region to State", "District to State")
  ) %>%
  ggplot(aes(x = complvl_diff_label, y = value)) +
  geom_col(aes(fill = comp, alpha = complvl_diff_label)) +
  scale_alpha_discrete(range = c(0.2, 1)) +
  guides(fill = FALSE, size = FALSE) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  facet_grid( ~ comp, scales = "free", labeller = label_wrap_gen(width = 12)) +
  labs(subtitle = "By Competition Level") +
  theme(axis.text.x = element_blank()) +
  labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_bycomplvl_diffs_2
#'
#'
#'
viz_comp_stats_bycomplvl_diffs_grid <-
  arrangeGrob(
    viz_comp_stats_bycomplvl_diffs,
    viz_comp_stats_bycomplvl_diffs_2 + labs(title = NULL, subtitle = NULL), # labs(title = "", subtitle = ""),
    nrow = 2
  )

grid.arrange(viz_comp_stats_bycomplvl_diffs_grid)
save_viz(viz_comp_stats_bycomplvl_diffs_grid)

#'
#' It appears that `Number Sense` demonstrates the largest
#' "jumps" in aggregate scores with increasing competition levels. Having competed
#' in this competition before, I do not find this all too suprising.
#' More than any other competition type, those who succeed `Number Sense`
#' rely on natural abilities (as opposed to training) to "beat out" the competition.
#' Often times, this "natural ability" can be deemed "savant"-like.
#' Consequently, with increasing competition level, is is more likely that these
#' "superior" competitors stand out, and, as observed here, skew
#' the scoring distributions higher with less "inferior" competitors to weight down the averages.
#'
#' ### Have the score distribution differences at each competition level changed over time?
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
lm(mean_diff_pct ~ mean + comp, data = comp_stats_byyear_diffs) %>% broom::tidy()

#'
#'
#'
viz_comp_stats_byyear_diffs <-
  comp_stats_byyear_diffs %>%
  ggplot(aes(x = year, y = mean_diff_pct)) +
  geom_line(aes(color = comp), size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  # scale_x_continuous(breaks = years_labs) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  guides(color = FALSE) +
  # geom_smooth(se = FALSE, color = "black", size = 1) +
  facet_grid(comp ~ ., scales = "free", labeller = label_wrap_gen(width = 12)) +
  labs(subtitle = "By Year") +
  labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_byyear_diffs
#'
#' Aside from a steep drop-off in the `Science` scores in 2014, there's
#' nothing too interesting about this visualization.
#'
#' As with the raw scores, there does not appear to be any discernable
#' trend in the change in level of difficulty of test between each competition level
#' over time. Again, this is a strong indiciation that the tests (and, most likely,
#' the skills of the competitors) have not changed over time.
#'
#'
#' There could be a number of confounding factors engendering the relatively even
#' competition level of time. For example, test makers may have made tests more difficult
#' if competitors were observed to have increased their skill levels. The converse
#' coulde be true as well. Or, most likely, test makers and competition skill level
#' have stayed relatively the same over time. If this is the case, at the very least
#' we can say that the academic "elite" of newer generations have not dropped off
#' in terms of academic prowess (although nothing can be dedcued about those
#' who do not compete in UIL academic competitions).
#'

#'
#'
#'
#+ include = FALSE
# analysis_specific ----

#'
#'
#' ## "Specific" Questions
#'
#' ### Which individuals competed in the most competitions?
#'
#'
#+ results = "hide"
comp_stats_byperson <-
  persons_all %>%
  group_by(name, school, conf) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  rank_and_arrange("cnt")
comp_stats_byperson
#'
#'
#'
comp_stats_byperson %>%
  slice(1:10) %>%
  mutate(
    cnt = color_tile("white", "orange")(cnt),
    cnt_bar = color_bar("lightgreen")(cnt)) %>%
  kable() %>%
  kable_styling(full_width = FALSE) %>%
  add_footnote(c(sprintf("# of total competitors = %d.", dim(comp_stats_byperson)[1])), notation = "symbol")

#'
#' There's not too much to "learn" from this ranking by simple count of
#' number of competitions competed in.
#' All I can say is that I'm jealous of these people.
#'
#'
#' ### From which conferences do the individuals who competed the most come?
#'
#+ results = "hide"
num_top <- 1000
comps_stats_byperson_top <-
  comp_stats_byperson %>%
  slice(1:num_top)

comps_stats_byperson_top_byconf <-
  comps_stats_byperson_top %>%
  group_by(conf) %>%
  # distinct(name, school) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  rank_and_arrange("cnt")
comps_stats_byperson_top_byconf
#'
#'
#'
comps_stats_byperson_top_byconf %>%
  mutate(
    cnt = color_tile("white", "orange")(cnt),
    cnt_bar = color_bar("lightgreen")(cnt)) %>%
  kable() %>%
  kable_styling(full_width = FALSE) %>%
  add_footnote(c(sprintf("# of total competitors included = %d.", dim(comps_stats_byperson_top_byconf)[1])), notation = "symbol")

#'
#' What stands out here is that there doesn't seem to be as many individual
#' competitors in `6A`. I would hypothesize that this might because the `6A` conference
#' has the largest schools, which are more likely to "allocate sparingly" their individual
#' competitors among different competitions. (There is a limit on the number of entrants
#' per school in a given competition. Smaller schools are much less likely to ever
#' have enough people to reach the per-school limit, so individuals at these schools
#' are welcom to compete in as many competitions as they would like.)
#'
#' ### Which individuals were most "dominant"?
#'
#' To rank the most "dominant" competitors, I use the sum of the
#' percentile rank of placings in all competitions (i.e. `prank_sum`).
#'
#' I evaluated some other metrics for gauging individual
#' success, including total number
#' of individuals defeated in head-to-head competitions (i.e. `defeat_cnt_sum`),
#' `prank` and `defeat_cnt` attempt to measure the same underlying thing,
#' but I think `prank` is a little more "natural" to interpret because
#' it contextualizes number of competitors implicitly with its "unit" value between 0 and 1,
#' whereas `defeat_cnt` can be difficult to interpret directly because
#' the number of head-to-head to competitors is not defined implicitly by it.
#' I also examed the  "average" versions of `prank` and `defeat_cnt`
#' (i.e. `prank_sum` and `defeat_cnt_sum`), but I found that they were sensitive
#' to individuals who did not compete in many competitions, yet placed very well
#' in them. This is not how I quantify "domination".
#'
#' I use `prank_sum` (or some slight modification of it) as the primary choice for ranking individuals and
#' schools in other parts of my analysis. (Raw `score` is also used, although
#' it is "naive".)
#'
#'
#+ results = "hide"
comp_stats_byperson_2 <-
  persons_all %>%
  group_by(name, school, conf) %>%
  summarise_at(vars(prank, defeat_cnt), funs(mean = mean, sum = sum)) %>%
  ungroup() %>%
  mutate(rank = row_number(desc(prank_sum))) %>%
  arrange(rank) %>%
  select(rank, everything())
comp_stats_byperson_2
comp_stats_byperson_2 %>%
  filter(name %in% c("Elhabr, Anthony", "Elhabr, Andrew"))
#'
#'
#'
comp_stats_byperson_2 %>% slice(1:10) %>% kable() %>% kable_styling(full_width = FALSE)

#'
#' Somewhat suprisingly, some of the same individuals from the raw "counts" ranking
#' of competitors also appeaer in the top of the ranks by my evaluation of domination.
#' Some additional statistical analysis could be done here to investigate
#' correlations.
#'
#' ### To what extent have some individual competitors "carried" their teams?
#'
#+ results = "hide"
# Apply filter and deselect comp for debugging.
top_pct <- 0.1
comp_stats_byperson_byschool <-
  persons_all %>%
  # filter(year == 2016) %>% # filter(conf == "1A") %>% filter(complvl == "state") %>% filter(comp_shortname == "num") %>%
  group_by(comp_shortname, complvl, conf, year, school) %>%
  mutate(ingroup_cnt = n()) %>%
  filter(ingroup_cnt > 3) %>%
  mutate(ingroup_rank = row_number(desc(score))) %>%
  filter(ingroup_rank %in% c(1, 2)) %>%
  arrange(desc(ingroup_rank)) %>%
  mutate(ingroup_diff = score - lag(score)) %>%
  filter(ingroup_rank == 1) %>%
  select(-ingroup_rank) %>%
  ungroup() %>%
  mutate(group_score = score - ingroup_diff) %>%
  mutate(among_group_rank = row_number(desc(ingroup_diff))) %>%
  # select(-comp) %>%
  arrange(among_group_rank)

comp_stats_byperson_byschool <-
  comp_stats_byperson_byschool %>%
  mutate(ingroup_diff_prank = percent_rank(ingroup_diff)) %>%
  # mutate(ingroup_diff_toppct = ifelse(ingroup_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(ingroup_diff_toppct = ifelse(ingroup_diff_prank >= (1 - top_pct), TRUE, FALSE))

comp_stats_byperson_byschool %>%
  select(
    among_group_rank,
    ingroup_diff,
    ingroup_diff_prank,
    ingroup_diff_toppct,
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
sample_pct <- 0.01

viz_comp_stats_byperson_byschool <-
  comp_stats_byperson_byschool %>%
  mutate(ingroup_diff_prank = percent_rank(ingroup_diff)) %>%
  # mutate(ingroup_diff_toppct = ifelse(ingroup_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(ingroup_diff_toppct = ifelse(ingroup_diff_prank >= (1 - top_pct), TRUE, FALSE)) %>%
  # slice(1:100) %>%
  sample_frac(size = sample_pct) %>%
  ggplot(aes(x = score, y = group_score)) +
  geom_point(aes(size = ingroup_diff, color = ingroup_diff_toppct)) +
  scale_color_manual(values = c("black", "red")) +
  geom_abline(
    aes(intercept = 0, slope = 1),
    color = "black",
    linetype = "sold",
    size = 2
  ) +
  geom_abline(aes(
    intercept = -quantile(ingroup_diff, 1 - top_pct),
    slope = 1
  ),
  color = "red",
  size = 1,
  linetype = "dashed") +
  labs(
    title = "Individual Score vs. Score of Team",
    caption = str_c(
      str_to_title("red"), " emphasizes top", sprintf("%d", 100 * top_pct), "% of individuals who \"carried\"."
    ),
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "none")
viz_comp_stats_byperson_byschool
save_viz(viz_comp_stats_byperson_byschool)
#'
#' This visualization only provides a glimpse at what could be done to investigate
#' this question. Anyways, I find it cool to see just how much some invidiuals
#' "rise above" the rest of their team.
#'
#'
#' ### Which siblings competed in the most competitions?
#'
#' Being a twin, the performance of siblings (some of which are most definitiely twins)
#' was something peculiar that I wanted to look at.
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
#+ results = "hide"
siblings_cnt <-
  siblings %>%
  group_by(name_last, name_first_pair) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  distinct(name_last, cnt, .keep_all = TRUE) %>%
  rank_and_arrange("cnt")
siblings_cnt

siblings_cnt_elhabr <-
  siblings_cnt %>%
  filter(name_last == "Elhabr")
#'
#'
#'
siblings_cnt %>% slice(1:10)

#'
#' I was dissapointed to find that my brother and I were not at
#' the very top of this list. Nevertheless, we rank relatively well (we
#' rank `r siblings_cnt_elhabr$rank`, having competed in
#' `r siblings_cnt_elhabr$cnt` competitions together).
#'
#' ### Which siblings were the most "dominant"?
#'
#+ results = "hide"
colnames_select <-
  names(siblings) %>%
  str_subset("^name|^prank")
colnames_gather <-
  names(siblings) %>%
  str_subset("^prank")

siblings_prank_sum <-
  siblings %>%
  select(one_of(colnames_select)) %>%
  gather(prank_sum_type, value, colnames_gather) %>%
  group_by(name_last, name_first_pair) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>%
  distinct(name_last, sum, .keep_all = TRUE) %>%
  mutate(rank = row_number(desc(sum))) %>%
  arrange(rank) %>%
  select(rank, everything())
siblings_prank_sum

siblings_prank_sum_elhabr <-
  siblings_prank_sum %>%
  filter(name_last == "Elhabr")
#'
#'
#'
siblings_prank_sum %>% slice(1:10)

#'
#' It looks like these rankings are fairly similar. My brother and I
#' are ranked `r siblings_prank_sum_elhabr$rank`, having aggregated a total percentile
#' rank of `r siblings_prank_sum_elhabr$sum` combined.
#'
#' ### How have people at my school performed?
#'
#' Aside from just my brother and me, I was interested in looking at the perfromance
#' of the people at my school. Although we weren't the top performaning siblings,
#' I was hoping to see us among the top of people at our school.
#'
#+ results = "hide"
comp_rank_byschool_byperson <-
  persons_all %>%
  group_by(school, name) %>%
  summarise(
    cnt = n(),
    prank_sum = sum(prank),
    prank_mean = mean(prank),
    defeat_cnt = sum(defeat_cnt)
  ) %>%
  ungroup() %>%
  rank_and_arrange("prank_sum")

comp_rank_byschool_byperson_clemens <-
  comp_rank_byschool_byperson %>%
  filter(school == "Clemens")
#'
#'
#'
comp_rank_byschool_byperson_clemens %>% select(-school) %>%  slice(1:10)

#'
#' Ranking by `prank_sum`,
#' it looks like my brother and I _are_ among the top performers
#' in our school's history.
#'
#' ### How has my school performed over time?
#'
#+ results = "hide"
comp_rank_byschool_byyear <-
  schools_all %>%
  mutate(state_cnt = ifelse(complvl == "regional" &
                              advanced == TRUE, TRUE, FALSE)) %>%
  group_by(school, year) %>%
  summarise(
    prank_sum = sum(prank),
    prank_mean = mean(prank),
    defeat_cnt = sum(defeat_cnt),
    advanced_cnt = sum(advanced),
    state_cnt = sum(state_cnt)
  ) %>%
  ungroup() %>%
  rank_and_arrange("prank_sum")

comp_rank_byschool_byyear_clemens <-
  comp_rank_byschool_byyear %>%
  filter(school == "Clemens")
#'
#'
#'
comp_rank_byschool_byyear_clemens %>% select(-school)

#'
#' Judging by `prank_sum`, it doesn't look like my
#' school has any consistent trend in performance by year.
#' In some years my school did well, and in others... not so much. In fact,
#' my school has _never_ made an appearnce in a `State` competitions!
#'
#'
#' ### Which schools have been the most "dominant"?
#'
#+ results = "hide"
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
  ungroup() %>%
  # mutate(rank = row_number(desc(prank_sum))) %>%
  # mutate_if(starts_with("prank"), funs(rank = row_number(desc(.)))) %>%
  # mutate(rank = prank_sum_rank)
  rank_and_arrange("prank_sum")

comp_rank_byschool_clemens <-
  comp_rank_byschool %>% filter(school == "Clemens")

#'
#'
#'
comp_rank_byschool %>% slice(1:10)

#'
#' I didn't expect to see my school among the most "dominant" schools. Nevertheless,
#' my school is ranked at `r comp_rank_byschool_clemens$rank`, which is respectable.
#'
#'
#'
#' # Conclusion
#'
#' That's all the analysis I'll do for now. There is _SOOOO_ much more that could
#' be explored. Alas, I'll leave that for another time.
#'



