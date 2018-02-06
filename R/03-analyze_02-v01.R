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
#+ include = FALSE
session::restore.session("R/analyze_01.RData")

#'
#'
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
    color = color_neutral_1,
    linetype = linetype_neutral_1,
    size = size_neutral_1
  ) +
  geom_vline(aes(xintercept = z_n1), color = color_neutral_1, linetype = linetype_neutral_2, size = size_neutral_2) +
  geom_vline(aes(xintercept = z_p1), color = color_neutral_1, linetype = linetype_neutral_2, size = size_neutral_2) +
  guides(fill = FALSE) +
  scale_x_continuous(limits = c(q05_min, q95_max)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # scale_y_continuuos(breaks = function(x) round(x, 2)) +
  facet_wrap(
    ~ comp,
    scales = "free",
    nrow = num_comps,
    labeller = label_wrap_gen(width = width_strip_wrap),
    strip.position = "right"
  ) +
  labs(caption = "mean = dashed line; z = -1 and z = 1 threshholds = solid lines") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs_comp_stats +
  theme_comp_stats

viz_comp_stats
# Error for some reason...
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
# year_min <- min(comp_stats_byyear$year)
# year_max <- max(comp_stats_byyear$year)
# years_labs <- seq(year_min, year_max, by = 4)

visualize_comp_stats_byx <- function(d, x_char = "", subtitle_suffix = "") {
  d %>%
    ggplot(
      aes_string(
        x = x_char,
        y = "mean"
      )
    ) +
    geom_pointrange(aes(ymin = z_n1, ymax = z_p1, color = comp)) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    guides(color = FALSE) +
    # Using facet_wrap() so strip.position can be specified.
    facet_wrap(
      ~ comp,
      scales = "free",
      nrow = num_comps,
      labeller = label_wrap_gen(width = width_strip_wrap),
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
  guides(fill = FALSE, size = FALSE) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  facet_grid( ~ comp, scales = "free", labeller = label_wrap_gen(width = width_strip_wrap)) +
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
#'
#'
#'
viz_comp_stats_byyear_diffs <-
  comp_stats_byyear_diffs %>%
  ggplot(aes(x = year, y = mean_diff_pct)) +
  geom_line(aes(color = comp), size = size_neutral_1) +
  geom_smooth(method = "lm", se = FALSE, color = color_neutral_1, linetype = linetype_neutral_2, size = size_neutral_2) +
  # scale_x_continuous(breaks = years_labs) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  guides(color = FALSE) +
  # geom_smooth(se = FALSE, color = "black", size = 1) +
  facet_grid(comp ~ ., scales = "free", labeller = label_wrap_gen(width = width_strip_wrap)) +
  labs(subtitle = "By Year") +
  labs_stats_byx_diffs +
  theme_comp_stats_byx_diffs
viz_comp_stats_byyear_diffs
#'
#'
#' As with the raw scores, there does not appear to be any discernable
#' trend in the change in level of difficulty of test between each competition level
#' over time. Again, this is a strong indiciation that the tests (and, most likely,
#' the skills of the competitors) have not changed over time.
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
#+ include = FALSE
session::save.session("R/analyze_02.RData")

#'
#'
#'

