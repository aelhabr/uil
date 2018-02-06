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
session::restore.session("R/analyze_02.RData")

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
comp_stats_byperson %>% slice(1:10)

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
comps_stats_byperson_top_byconf

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
comp_stats_byperson_2 %>% slice(1:10)

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

comp_stats_byperson_byschool <-
  comp_stats_byperson_byschool %>%
  mutate(within_group_diff_prank = percent_rank(within_group_diff)) %>%
  # mutate(within_group_diff_toppct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(within_group_diff_toppct = ifelse(within_group_diff_prank >= (1 - top_pct), TRUE, FALSE))

comp_stats_byperson_byschool %>%
  select(
    among_group_rank,
    within_group_diff,
    within_group_diff_prank,
    within_group_diff_toppct,
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
  mutate(within_group_diff_prank = percent_rank(within_group_diff)) %>%
  # mutate(within_group_diff_toppct = ifelse(within_group_diff_prank >= 0.9, TRUE, FALSE)) %>%
  mutate(within_group_diff_toppct = ifelse(within_group_diff_prank >= (1 - top_pct), TRUE, FALSE)) %>%
  # slice(1:100) %>%
  sample_frac(size = sample_pct) %>%
  ggplot(aes(x = score, y = group_score)) +
  geom_point(aes(size = within_group_diff, color = within_group_diff_toppct)) +
  scale_color_manual(values = c("black", color_neutral_2)) +
  geom_abline(
    aes(intercept = 0, slope = 1),
    color = color_neutral_1,
    linetype = linetype_neutral_1,
    size = size_neutral_1
  ) +
  geom_abline(aes(
    intercept = -quantile(within_group_diff, 1 - top_pct),
    slope = 1
  ),
  color = color_neutral_2,
  size = size_neutral_2,
  linetype = linetype_neutral_2) +
  labs(
    title = "Individual Score vs. Score of Team",
    caption = str_c(
      str_to_title(color_neutral_2), " emphasizes top", sprintf("%d", 100 * top_pct), "% of individuals who \"carried\"."
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
#'
#+ include = FALSE
session::save.session("R/analyze_03.RData")

#'
#'
#'

