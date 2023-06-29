# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------
# ---------------------- Zorgmijding project: Calculate denominators -----------
# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------


# For all questions contact Premysl Velek at p.velek@erasmusmc.nl


#' denominators
#'
#' Takes the cleaned patient data - saved by the clean_data function - to calculate
#' the population size for each period -  month, week and day - from 2016 to 2020.
#' The results are automatically saved in the results' folder created as a subfolder
#' in the folder in which the raw data are located.
#'
#' There are no parameters

#' @importFrom svDialogs dlg_message
#' @importFrom tcltk tk_choose.files
#' @importFrom fs path_dir
#' @import purrr
#' @import lubridate
#' @import dplyr

#' @param age_groups Definition of age groups stratification by age. Has to be
#' identical to 'age_groups' parameter used in the clean_data function !!
#' @param exclude Age groups to be excluded if we want to only consider older population


#' @return the path to the resulting dataframes

#' @export

denominators <- function(age_groups = c("0-19", "20-39", "40-59", "60-79", "80+"),
                         exclude = c("0-19", "20-39")) {
  # Paths to raw data ------------------------------
  svDialogs::dlg_message("Please select the cleaned patient data. Continue by pressing OK.
              \n
              Please note that this function may need a very long time to run.",
              type = c("ok"), gui = .GUI
  )
  path_patients <- tcltk::tk_choose.files(caption = "Select patient data")

  patients <- readRDS(path_patients)

  # get the latest follow up date for each periodicity
  max_fu_end_day <- max(patients$fu_end)
  max_fu_end_month <- floor_date(max_fu_end_day, unit = "month") + days(14)
  max_fu_end_week <- floor_date(max_fu_end_day, unit = "week",
                               week_start = getOption("lubridate.week.start", 1))

  # Calculation  ------------------------------
  weeks <- seq(lubridate::ymd("2016-01-04"), max_fu_end_week, by = "weeks")
  months <- seq(lubridate::ymd("2016-01-15"), max_fu_end_month, by = "months")
  days <- seq(lubridate::ymd("2016-01-01"), max_fu_end_day, by = "days")

  print("\n= Counting monthly number of patients in the dabatase =\n")

  # size by month
  db_size_m_sex_age <- lapply(months, count_patients, df = patients, .age_groups = age_groups) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows)

  cat("\n= Counting weekly number of patients in  the dabatase =\n")

  # size by week
  db_size_w_sex_age <- lapply(weeks, count_patients, df = patients, .age_groups = age_groups) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows)

  cat("\n= Counting daily number of patients in the dabatase =\n")

  # size by days
  db_size_d_sex_age <- lapply(days, count_patients, df = patients, .age_groups = age_groups) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows)

  cat("= Almost done ... =")

  # aggregate by age
  db_size_m_age <- db_size_m_sex_age %>%
    dplyr::group_by(date, age_g) %>%
    dplyr::summarise(size = sum(size))

  db_size_w_age <- db_size_w_sex_age %>%
    dplyr::group_by(date, age_g) %>%
    dplyr::summarise(size = sum(size))

  db_size_d_age <- db_size_d_sex_age %>%
    dplyr::group_by(date, age_g) %>%
    dplyr::summarise(size = sum(size))


  # aggregate by sex
  db_size_m_sex <- db_size_m_sex_age %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))

  db_size_w_sex <- db_size_w_sex_age %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))

  db_size_d_sex <- db_size_d_sex_age %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))


  # aggregate over age and sex: all patients!
  db_size_m_total <- db_size_m_sex_age %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))

  db_size_w_total <- db_size_w_sex_age %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))

  db_size_d_total <- db_size_d_sex_age %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))


  # aggregate by sex : only patients over maximum age defined in the exlude parameter
  db_size_m_sex_40 <- db_size_m_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))

  db_size_w_sex_40 <- db_size_w_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))

  db_size_d_sex_40 <- db_size_d_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date, sex) %>%
    dplyr::summarise(size = sum(size))


  # aggregate over age and sex
  db_size_m_total_40 <- db_size_m_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))

  db_size_w_total_40 <- db_size_w_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))

  db_size_d_total_40 <- db_size_d_sex_age %>%
    dplyr::filter(!(age_g %in% exclude)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(size = sum(size))


  # save to RData ------------------------------------------------------------------------------

  path_denominators <- file.path(fs::path_dir(path_patients), "denominators.RData")


  # save cleaned data
  save(db_size_m_sex_age,
       db_size_w_sex_age,
       db_size_d_sex_age,
       db_size_m_age,
       db_size_w_age,
       db_size_d_age,
       db_size_m_sex,
       db_size_w_sex,
       db_size_d_sex,
       db_size_m_total,
       db_size_w_total,
       db_size_d_total,
       db_size_m_sex_40,
       db_size_w_sex_40,
       db_size_d_sex_40,
       db_size_m_total_40,
       db_size_w_total_40,
       db_size_d_total_40,
       file = path_denominators
  )

  message(paste(
    "Counting successful! \nResults have been saved into the following folder:",
    file.path(fs::path_dir(path_patients), "results")
  ))

  path_denominators <- file.path(fs::path_dir(path_patients), "denominators.RData")

  # return paths to the cleaned dataframes
  return(path_denominators)
}



# helper function to  count the number of patients for each week, day and month
count_patients <- function(date, df, .age_groups) {

  # get breaks from the user defined age groups
  br <- as.numeric(substring(.age_groups, nchar(.age_groups) - 1)[-length(.age_groups)]) + 1


  df %>%
    dplyr::mutate(
      age = (lubridate::interval(pat_dob, date)) / lubridate::years(1),
      age_g = factor(
        cut(age,
            breaks = c(-Inf, br, Inf),
            labels = .age_groups
        ),
        levels = .age_groups
      )
    ) %>%
    dplyr::group_by(age_g, sex, .drop = FALSE) %>%
    dplyr::summarise(size = sum(lubridate::`%within%`(date, lubridate::interval(fu_start, fu_end)))) %>%
    dplyr::mutate(date = date)
}
