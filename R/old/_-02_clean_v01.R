

clean_schools <-
  function(raw,
           complvl,
           default_city = "unknown",
           default_complvl_num = 0) {
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

    edited_1 <- renamed

    # z <- edited_1 %>% filter(school_city_complvlnum %in% c("Academy H S (32)"))
    # z
    # z %>% mutate(a = ifelse(
    #   str_detect(school_city_complvlnum, "[S]\\s\\(.*\\)$") == TRUE,
    #   str_replace(school_city_complvlnum, "[S]\\s\\(", str_c("S, ", default_city, " \\(")),
    #   school_city_complvlnum
    # ))

    # Fix cases where 'HS' is not followed by a comma or city, such as 'Academy H S (32)'.
    edited_1a <-
      edited_1 %>%
      mutate(
        school_city_complvlnum = ifelse(
          str_detect(school_city_complvlnum, "[S]\\s\\(.*\\)$") == TRUE,
          str_replace(
            school_city_complvlnum,
            "[S]\\s\\(",
            str_c("S, ", default_city, " \\(")
          ),
          school_city_complvlnum
        )
      )

    edited_1a %>% filter(school_city_complvlnum %in% c("Academy H S (32)"))
    setdiff(edited_1a, edited_1)

    # Remove 'H S' from high school name.
    # Note that some schools have 'Hs' instead. which isn't captured by this regex.
    # This is done on purpose, because the alternative regex conflicts with school names where some
    # combination of the letters 'h' and 's' are found consecutively.
    # A fix is made later.
    # edited_1b <-
    #   edited_1a %>%
    #   mutate(school_city_complvlnum = str_replace(school_city_complvlnum, "\\s[H]\\s[Ss]", ""))
    # # mutate(school_city_complvlnum = str_replace(school_city_complvlnum, "\\s[H].*[Ss]", ""))

    # Separate school_city_complvlnum.
    edited_2 <-
      # edited_1b %>%
      edited_1a %>%
      separate(school_city_complvlnum,
               c("school", "city_complvlnum"),
               sep = ", ")

    # edited_2 %>% pull(city_complvlnum) %>% str_subset("^\\(")

    # Fix cases where no city is listed, so city_complvl looks something like '(32)'.
    edited_2a <-
      edited_2 %>%
      mutate(city_complvlnum = ifelse(
        str_detect(city_complvlnum, "^\\(") == TRUE,
        str_replace(city_complvlnum, "^\\(", str_c(default_city, " \\(")),
        city_complvlnum
      ))
    # setdiff(edited_2a, edited_2)

    if (complvl == "state") {
      edited_3 <-
        edited_2a %>%
        mutate(city_complvlnum = str_c(city_complvlnum, str_c(
          " \\(", as.character(default_complvl_num), "\\)"
        )))
    } else {
      edited_3 <- edited_2a
    }

    output <-
      edited_3 %>%
      separate(city_complvlnum, c("city", "complvlnum"), sep = "\\s\\(") %>%
      mutate(complvlnum = str_replace(complvlnum, "\\)", "")) %>%
      mutate(complvlnum = as.numeric(complvlnum)) %>%
      rename(complvl_num = complvlnum)

    output
  }
