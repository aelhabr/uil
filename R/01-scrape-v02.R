


#'
#'
#'
#+ include = FALSE
rm(list = ls())

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("tidyr")
library("rvest")

#'
#'
#'
# Functions. ----
# source("R/functions_predictiontracker.R")

#'
#'
#'
# Parameters. ----
export <- TRUE
create_backup <- TRUE
timestamp_download <- Sys.time()
timestamp_download_char <-
  format(timestamp_download, "%Y-%m-%d_%H-%M-%S")


dir_download <- str_c(getwd(), "/data/")
filename_download <- "temp"
filename_download_ext <- ".html"
filepath_download <-
  str_c(dir_download, filename_download, filename_download_ext)

if (export == TRUE) {
  dir_export <- str_c(getwd(), "/data/")
  # filename_export_base <- "results"
  filename_export_suffix <- "-scraped"
  filename_export_ext <- ".csv"
  filepath_export_schools <-
    str_c(dir_export,
          "schools",
          filename_export_suffix,
          filename_export_ext)

  filepath_export_persons <-
    str_c(dir_export,
          "persons",
          filename_export_suffix,
          filename_export_ext)

  if (create_backup == TRUE) {
    filepath_export_schools_ts <-
      str_c(
        dir_export,
        "schools",
        filename_export_suffix,
        "-",
        timestamp_download_char,
        filename_export_ext
      )

    filepath_export_persons_ts <-
      str_c(
        dir_export,
        "persons",
        filename_export_suffix,
        "-",
        timestamp_download_char,
        filename_export_ext
      )

  }

}
#'
#'
#'
# More parameters. ----
years <- seq(2004, 2017)
confs <- str_c(seq(1, 6), "A")
complvls <- c("district", "regional", "state")
comps_list <-
  list(
    cal = "Calculator Applications",
    csc = "Computer Science",
    mth = "Mathematics",
    num = "Number Sense",
    sci = "Science"
  )
comps <- names(comps_list)

# years_scrape <- seq(2010, 2016)
# confs_scrape <- c("4A", "5A", "6A")
# complvls_scrape <- complvls
# comps_scrape <- c("cal", "mth", "num")

years_scrape <- years
confs_scrape <- confs
complvls_scrape <- complvls
comps_scrape <- comps

# url_test <- "https://www.hpscience.net/results/2012/4A/district/num.php"
url_base <- "https://www.hpscience.net/results/"
url_suffix <- ".php"

rt <- robotstxt::robotstxt("www.hpscience.net")
rt$bots
rt$permissions
rt$check()
rt$crawldelay %>% tbl_df()

clean <-
  function(raw,
           complvl) {
    # raw <- schools
    # raw <- persons
    renamed <-
      raw %>%
      as_tibble() %>%
      janitor::clean_names()

    if (complvl == "district") {
      renamed <-
        renamed %>% rename(school_city_complvlnum = school_city_district)
    } else if (complvl == "regional") {
      renamed <-
        renamed %>% rename(school_city_complvlnum = school_city_region)
    } else if (complvl == "state") {
      renamed <- renamed %>% rename(school_city_complvlnum = school_city)
    } else {
      stop()
    }
    renamed
  }
#'
#'
#'
# Loop. ----
i <- 1
j <- 1
k <- 1
l <- 1
while (i <= length(years_scrape)) {
  # {}

  year <- years_scrape[i]
  # conf <- 2012
  j <- 1
  k <- 1
  l <- 1

  while (j <= length(confs_scrape)) {
    # {}

    conf <- confs_scrape[j]
    # conf <- "4A"
    k <- 1
    l <- 1

    while (k <= length(complvls_scrape)) {
      # {}

      complvl <- complvls_scrape[k]
      # complvl <- "state"
      l <- 1

      while (l <= length(comps_scrape)) {
        # {}

        comp <- comps_scrape[l]
        # comp <- "mth"

        url <-
          str_c(url_base,
                year,
                "/",
                conf,
                "/",
                complvl,
                "/",
                comp,
                url_suffix)

        # Test district page.
        # url <- "https://www.hpscience.net/results/2005/1A/district/cal.php"
        # Test district page with weird school names.
        # url <- "https://www.hpscience.net/results/2005/1A/district/sci.php"
        # Test state.
        # complvl <- "state"
        # url <- "https://www.hpscience.net/results/2005/1A/state/sci.php"

        download.file(url, destfile = filepath_download)

        results_list <-
          filepath_download %>%
          read_html() %>%
          # html_nodes("table")
          html_nodes(xpath = "/html/body/table") %>%
          html_table(header = TRUE)

        schools <- results_list[[1]]
        persons <- results_list[[2]]

        schools_cleaned <-
          schools %>%
          clean(complvl)

        persons_cleaned <-
          persons %>%
          clean(complvl)

        schools_mutated <-
          schools_cleaned %>%
          mutate(
            year = year,
            conf = conf,
            complvl = complvl,
            comp = comp,
            url = url
          )

        persons_mutated <-
          persons_cleaned %>%
          mutate(
            year = year,
            conf = conf,
            complvl = complvl,
            comp = comp,
            url = url
          )

        if (i == 1) {
          schools_all <- schools_mutated
          persons_all <- persons_mutated
        } else {
          schools_all <- bind_rows(schools_all, schools_mutated)
          persons_all <- bind_rows(persons_all, persons_mutated)
        }


        l <- l + 1

      }
      k <- k + 1
    }
    j <- j + 1
  }
  i <- i + 1
}

#'
#'
#'


#'
#'
#'
# Export. ----

if (export == TRUE) {
  readr::write_csv(schools_all, filepath_export_schools)
  readr:write_csv(persons_all, filepath_export_persons)

  if (create_backup == TRUE) {
    file.copy(filepath_export_schools, filepath_export_schools_ts)
    file.copy(filepath_export_persons, filepath_export_persons_ts)
  }
}

#'
#'
#'
