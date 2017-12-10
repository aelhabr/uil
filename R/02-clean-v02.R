
#'
#'
#'
rm(list = ls())

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("tidyr")

#'
#'
#'
# Parameters. ----
export <- TRUE

dir_import <- str_c("data/")
# filename_import_base <- "results"
filename_import_suffix <- "-scraped"
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

if (export == TRUE) {
  dir_export <- str_c(getwd(), "/data/")
  # filename_export_base <- "results"
  filename_export_suffix <- "-cleaned"
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
  }
#'
#'
#'
# Constants. ----
# comps_list <-
#   list(
#     cal = "Calculator Applications",
#     csc = "Computer Science",
#     mth = "Mathematics",
#     num = "Number Sense",
#     sci = "Science"
#   )

comps_labels_valid <- c("cal", "csc", "mth", "num", "sci")
comp_names_valid <-
  c(
    "Calculator Applications",
    "Computer Science",
    "Mathematics",
    "Number Sense",
    "Science"
  )

comps_valid <- tibble(comp = comps_labels_valid, comp_name = comp_names_valid)
# comps_valid

complvls_valid <- c("District", "Region", "State")
#'
#'
#'
# Import. ----
schools_all <- readr::read_csv(filepath_import_schools)
persons_all <- readr::read_csv(filepath_import_persons)

clean_schools <-
  function(raw,
           default_city = "unknown",
           default_complvl_num = 0) {

    default_city = "unknown"
    default_complvl_num = 0
    # raw <- schools_all
    # raw <- persons_all

    edited_0 <-
      raw %>%
      select(-url) %>%
      mutate(school_city_complvlnum_raw = school_city_complvlnum)

    # Remove commas in the middle of a school name.
    regex_pattern_detect <- ",\\s(Science)"
    regex_pattern <- regex_pattern_detect
    regex_replacement <- " Science"
    edited_1a <-
      edited_0 %>%
      mutate(
        school_city_complvlnum = ifelse(
          str_detect(school_city_complvlnum, regex_pattern_detect) == TRUE,
          str_replace(
            school_city_complvlnum,
            regex_pattern,
            regex_replacement
          ),
          school_city_complvlnum
        )
      )

    # setdiff(edited_1a, edited_0)
    # setdiff(edited_0, edited_1a)

    # Fix cases where 'HS' is not followed by a comma or city, such as 'Academy H S (32)'.
    # This is only a problem with the persons data.
    regex_pattern_detect <- "[S]\\s\\(.*\\)$"
    regex_pattern <- "[S]\\s\\("
    regex_replacement <- str_c("S, ", default_city, " \\(")
    edited_1b <-
      edited_1a %>%
      mutate(
        school_city_complvlnum = ifelse(
          str_detect(school_city_complvlnum, regex_pattern_detect) == TRUE,
          str_replace(
            school_city_complvlnum,
            regex_pattern,
            regex_replacement
          ),
          school_city_complvlnum
        )
      )

    # edited_1b %>% filter(school_city_complvlnum %in% c("Academy H S (32)"))
    # setdiff(edited_1b, edited_1a)

    # Remove 'H S' from high school name.
    # Note that some schools have 'Hs' instead. which isn't captured by this regex.
    # This is done on purpose, because the alternative regex conflicts with school names where some
    # combination of the letters 'h' and 's' are found consecutively.
    # A fix is made later.
    regex_pattern <- "\\s[H]\\s*[Ss]" # "\\s[H]\\s[Ss]"

    regex_replacement <- ""
    edited_1c <-
      edited_1b %>%
      mutate(school_city_complvlnum = str_replace(school_city_complvlnum, regex_pattern, regex_replacement))

    # regex_pattern2 <- "\\s[H]\\s[Ss]"
    # edited_1c2 <-
    #   edited_1b %>%
    #   mutate(school_city_complvlnum = str_replace(school_city_complvlnum, regex_pattern2, regex_replacement))
    #
    # setdiff(edited_1c, edited_1c2)
    # setdiff(edited_1c2, edited_1c)
    # setdiff(edited_1c, edited_1b)
    # setdiff(edited_1b, edited_1c)

    # Separate school_city_complvlnum.
    edited_2a <-
      edited_1c %>%
      # edited_1a %>%
      separate(school_city_complvlnum,
               c("school", "city_complvlnum"),
               sep = ", ") %>%
      select(-school_city_complvlnum_raw)


    # # edited_2 %>%
    # # persons_all %>%
    # schools_all %>%
    #   mutate(rn = row_number()) %>%
    #   filter(rn %in% c(4789, 4887, 4888, 4896, 10086, 10091, 10095, 10098, 15463, 17713, 23042, 28373, 28387, 33711, 39062, 39072, 43487))
    #   # filter(rn %in% c(30430, 30761, 30982, 41555, 46915, 47118, 52053, 52396, 52593))
    #   filter(rn %in% c(5339, 5434, 5435, 5436, 5437, 5438, 10062, 10063, 10064, 10066, 10067, 10068, 14206, 14207))
    # # edited_2 %>% pull(city_complvlnum) %>% str_subset("^\\(")

    # Fix cases where no city is listed, so city_complvl looks something like '(32)'.
    edited_2b <-
      edited_2a %>%
      mutate(city_complvlnum = ifelse(
        str_detect(city_complvlnum, "^\\(") == TRUE,
        str_replace(city_complvlnum, "^\\(", str_c(default_city, " \\(")),
        city_complvlnum
      ))
    # setdiff(edited_2a, edited_2)

    # Add dummy city value for 'state' complvl.
    default_complvl_num_char <- as.character(default_complvl_num)
    edited_3 <-
      edited_2b %>%
      mutate(city_complvlnum =
               ifelse(
                 complvl == "state",
                 str_c(city_complvlnum, str_c(
                   " (", default_complvl_num_char, ")"
                 )),
                 city_complvlnum
               ))

    # edited_3 %>%
    # # # persons_all %>%
    # # schools_all %>%
    #   mutate(rn = row_number()) %>%
    #   filter(rn %in% c(1087, 1088, 1089, 1090, 1091, 1092, 1093, 10947))

    edited_4 <-
      edited_3 %>%
      separate(city_complvlnum, c("city", "complvlnum"), sep = "\\s\\(") %>%
      mutate(complvlnum = str_replace(complvlnum, "\\)", "")) %>%
      mutate(complvlnum = as.numeric(complvlnum)) %>%
      rename(complvl_num = complvlnum) %>%
      mutate(complvl = ifelse(complvl == "regional", "region", complvl)) %>%
      mutate(complvl = str_to_title(complvl))

    output <-
      edited_4 %>%
      inner_join(comps_valid) %>%
      mutate_at(vars(conf, complvl, comp, comp_name), funs(as.factor)) %>%
      mutate(complvl = str_to_title(complvl)) %>%
      rename(comp_shortname = comp) %>%
      rename(comp = comp_name)

    output
  }

schools_cleaned <- clean_schools(schools_all)
persons_cleaned <- clean_schools(persons_all)

# These are difficult cases to account for...
schools_cleaned %>% filter(is.na(complvl_num))
persons_cleaned %>% filter(is.na(complvl_num))
#'
#'
#'
# Export. ----

if (export == TRUE) {
  readr::write_csv(schools_cleaned, filepath_export_schools)
  readr::write_csv(persons_cleaned, filepath_export_persons)
}


