# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------
# ---------------------- Zorgmijding project: count visits ---------------------
# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------

# For all questions contact Premysl Velek at p.velek@erasmusmc.nl


#' n_visits
#'
#' Takes the cleaned visit data (output of the clean_data function) and the denominators data (output of
#' the denominators function) and counts the number of GP contacts per period (month, week, day).

#' @importFrom svDialogs dlg_message
#' @importFrom tcltk tk_choose.files
#' @importFrom fs path_dir
#' @importFrom tidyr replace_na
#' @import lubridate
#' @import dplyr



#' @param averages an indicator whether a prepandemic averages should be computed. If set to TRUE
#'   (the default), then the output is a dataset in which there are two numbers for each period
#'   (month, week, day) in a year: one is the pre-pandemic average (2017-2019), and the other is the
#'   observed values in 2020. If set to FALSE, then data for the entire period (2016-2020) is
#'   returned.
#' @param filename name of the file in which the results will be stored and saved
#'
#' @param remove_counts if set to TRUE (the default), the resulting dataframes will not contain the
#'   absolute counts, only the rates will be returned
#'
#' @return list with paths to the resulting data

#' @export
n_visits <- function(averages = TRUE, filename = "results", remove_counts = TRUE) {
  if (!is.logical(averages) | !is.logical(remove_counts)) {
    stop("The arguments 'averages' and 'remove_counts' have to be either TRUE or FALSE")
  }


  # Paths to data ------------------------------
  svDialogs::dlg_message("Please select the cleaned visit data. Continue by pressing OK.",
    type = c("ok"), gui = .GUI
  )
  path_visits <- tcltk::tk_choose.files(caption = "Select visit data")


  svDialogs::dlg_message("Please select the denominators data. Continue by pressing OK.",
    type = c("ok"), gui = .GUI
  )
  path_denominators <- tcltk::tk_choose.files(caption = "Select denominators data")

  e <- new.env()
  load(path_denominators, envir = e)
  visits <- readRDS(path_visits)

  # pipes to reuse
  calculate <- . %>%
    dplyr::rename(date = !!date) %>%
    dplyr::filter(date > "2016-12-31") %>%
    dplyr::group_by_at(by, .drop = FALSE) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::left_join(., e[[ paste("db_size_",
      dplyr::if_else(nchar(date) < 11, substr(date, 1, 1), substr(date, 9, 9)), "_",
      strata,
      sep = ""
    )]], by = by) %>%
    tidyr::replace_na(list(n = 0)) %>%

    # remove rows in which size is NA which is caused by missing corresponding
    #  sample size in the denominators file (the follow up ended before the contact_date)
    dplyr::filter(!is.na(size)) %>%

    dplyr::mutate(
      rate = round((n / size) * 100000, 1),
      prepan = factor(dplyr::if_else(lubridate::year(date) < 2020,
        "3-year average\nprepandemic",
        as.character(lubridate::year(date))
      ))
    ) %>%
    # if both n and size is zero for some combination of covariates, then the rate is NaN.
    # we need to replace it with zero
    dplyr::mutate(rate = replace(rate, is.nan(rate), 0))

  calculate_avrg <- . %>%
    dplyr::mutate(year_date = format(date, format = date_f, locale = "English")) %>%
    dplyr::group_by_at(c("prepan", "year_date", by[-1])) %>%
    dplyr::summarise(

      # make sure it is a numeric class to avoid integer overflow
      rate = stats::weighted.mean(rate, size * 0.1),
      n = stats::weighted.mean(n, size * 0.1)
    ) %>%
    # same here, if rates or n for all years are zero for some combination of covariates,
    # then the  mean rate is NaN. We need to replace it with zero
    dplyr::mutate(
      rate = replace(rate, is.nan(rate), 0),
      n = replace(n, is.nan(n), 0)
    )

  Sys.setlocale("LC_TIME", "en-GB")

  # month
  date_month <- "monthyear"
  date_f_month <- "%b"

  # week
  date_week <- "week_start"
  date_f_week <- "%V"

  # day
  date_day <- "contact_date"
  date_f_day <- "%V %a"


  # visits by age and sex ---------------
  by <- c("date", "age_g", "sex")
  strata <- "sex_age"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_sex_age <- visits %>% calculate()
  if (averages) {
    n_visits_m_sex_age <- n_visits_m_sex_age %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_sex_age <- visits %>% calculate()
  if (averages) {
    n_visits_w_sex_age <- n_visits_w_sex_age %>% calculate_avrg()
  }


  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_sex_age <- visits %>% calculate()
  if (averages) {
    n_visits_d_sex_age <- n_visits_d_sex_age %>% calculate_avrg()
  }


  # visits by age ---------------
  by <- c("date", "age_g")
  strata <- "age"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_age <- visits %>% calculate()
  if (averages) {
    n_visits_m_age <- n_visits_m_age %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_age <- visits %>% calculate()
  if (averages) {
    n_visits_w_age <- n_visits_w_age %>% calculate_avrg()
  }

  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_age <- visits %>% calculate()
  if (averages) {
    n_visits_d_age <- n_visits_d_age %>% calculate_avrg()
  }


  # visits by sex ---------------
  by <- c("date", "sex")
  strata <- "sex"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_sex <- visits %>% calculate()
  if (averages) {
    n_visits_m_sex <- n_visits_m_sex %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_sex <- visits %>% calculate()
  if (averages) {
    n_visits_w_sex <- n_visits_w_sex %>% calculate_avrg()
  }

  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_sex <- visits %>% calculate()
  if (averages) {
    n_visits_d_sex <- n_visits_d_sex %>% calculate_avrg()
  }

  # visits total ----------------
  by <- "date"
  strata <- "total"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_total <- visits %>% calculate()
  if (averages) {
    n_visits_m_total <- n_visits_m_total %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_total <- visits %>% calculate()
  if (averages) {
    n_visits_w_total <- n_visits_w_total %>% calculate_avrg()
  }

  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_total <- visits %>% calculate()
  if (averages) {
    n_visits_d_total <- n_visits_d_total %>% calculate_avrg()
  }

  # visits by sex: 40 and older
  by <- c("date", "sex")
  strata <- "sex_40"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_sex_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_m_sex_40 <- n_visits_m_sex_40 %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_sex_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_w_sex_40 <- n_visits_w_sex_40 %>% calculate_avrg()
  }

  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_sex_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_d_sex_40 <- n_visits_d_sex_40 %>% calculate_avrg()
  }

  # visits total: 40 and older ----------------
  by <- "date"
  strata <- "total_40"

  # month
  date <- date_month
  date_f <- date_f_month

  n_visits_m_total_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_m_total_40 <- n_visits_m_total_40 %>% calculate_avrg()
  }

  # week
  date <- date_week
  date_f <- date_f_week

  n_visits_w_total_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_w_total_40 <- n_visits_w_total_40 %>% calculate_avrg()
  }

  # day
  date <- date_day
  date_f <- date_f_day

  n_visits_d_total_40 <- visits %>%
    dplyr::filter(!age_g %in% c("0-19", "20-39")) %>% calculate()
  if (averages) {
    n_visits_d_total_40 <- n_visits_d_total_40 %>% calculate_avrg()
  }



  # save to RData ------------------------------------------------------------------------------

  # create a directory to save the resulting data
  if (!dir.exists(file.path(fs::path_dir(path_visits), "results"))) {
    dir.create(file.path(fs::path_dir(path_visits), "results"), recursive = TRUE)
  }

  path_results <- file.path(fs::path_dir(path_visits), "results", paste0(filename, ".RData"))

  if (remove_counts) {
      n_visits_m_sex_age <- n_visits_m_sex_age %>% dplyr::select(-n)
      n_visits_w_sex_age <- n_visits_w_sex_age %>% dplyr::select(-n)
      n_visits_d_sex_age <- n_visits_d_sex_age %>% dplyr::select(-n)
      n_visits_m_age <- n_visits_m_age %>% dplyr::select(-n)
      n_visits_w_age <- n_visits_w_age %>% dplyr::select(-n)
      n_visits_d_age <- n_visits_d_age %>% dplyr::select(-n)
      n_visits_m_sex <- n_visits_m_sex %>% dplyr::select(-n)
      n_visits_w_sex <- n_visits_w_sex %>% dplyr::select(-n)
      n_visits_d_sex <- n_visits_d_sex %>% dplyr::select(-n)
      n_visits_m_sex_40 <- n_visits_m_sex_40 %>% dplyr::select(-n)
      n_visits_w_sex_40 <- n_visits_w_sex_40 %>% dplyr::select(-n)
      n_visits_d_sex_40 <- n_visits_d_sex_40 %>% dplyr::select(-n)
      n_visits_m_total <- n_visits_m_total %>% dplyr::select(-n)
      n_visits_w_total <- n_visits_w_total %>% dplyr::select(-n)
      n_visits_d_total <- n_visits_d_total %>% dplyr::select(-n)
      n_visits_m_total_40 <- n_visits_m_total_40 %>% dplyr::select(-n)
      n_visits_w_total_40 <- n_visits_w_total_40 %>% dplyr::select(-n)
      n_visits_d_total_40 <- n_visits_d_total_40 %>% dplyr::select(-n)
  }

  # save cleaned data
  save(n_visits_m_sex_age,
    n_visits_w_sex_age,
    n_visits_d_sex_age,
    n_visits_m_age,
    n_visits_w_age,
    n_visits_d_age,
    n_visits_m_sex,
    n_visits_w_sex,
    n_visits_d_sex,
    n_visits_m_sex_40,
    n_visits_w_sex_40,
    n_visits_d_sex_40,
    n_visits_m_total,
    n_visits_w_total,
    n_visits_d_total,
    n_visits_m_total_40,
    n_visits_w_total_40,
    n_visits_d_total_40,
    file = path_results
  )

  message(
    paste(
      "Counting successful! \nFinal dataframes have been saved into the following folder:",
      fs::path_dir(path_results)
    )
  )
  path_results
}
