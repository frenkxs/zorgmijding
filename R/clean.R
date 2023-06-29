# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------
# ---------------------- Zorgmijding project: load and clean data --------------
# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------

# For all questions contact Premysl Velek at p.velek@erasmusmc.nl


#' clean_data
#'
#' Takes raw data from an sql output and format and clean it for further analysis This is the fist
#' function in this pipeline, it should be run first. It makes sure the variables are consistently
#' named in the same order and have the right format. It also check there are no missing or
#' nonsensical data. It ask the user to provide two raw data files: the one with GP contacts and
#' one with all patients. The cleaned data are automatically saved in the 'results' folder created
#' as a subfolder in the folder in which the raw data are loca ted.


#' @importFrom svDialogs dlg_message
#' @importFrom tcltk tk_choose.files
#' @importFrom fs path_dir
#' @importFrom magrittr "%>%"
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom anytime anydate
#' @import lubridate
#' @import dplyr


#' @param umc University medical centre that provides the data. It can take the
#' the following values: "utrecht", "maastricht", "amsterdam", "groningen", "rotterdam"
#'
#' @return list with paths to the cleaned dataframes: visits and patients

#' @export
clean_data <- function(umc = c(
                         "utrecht", "maastricht", "amsterdam",
                         "groningen", "rotterdam"
                       )) {
  # Preliminaries ---------------------------------------
  umc <- match.arg(umc)


  # Paths to raw data ------------------------------

  # Visit data
  svDialogs::dlg_message("Please select the raw data file containing all cardiovascular GP contacts.
              Continue by pressing OK",
    type = c("ok"), gui = .GUI
  )
  path_visits <- tcltk::tk_choose.files(caption = "Select visit table")


  # Patient data
  svDialogs::dlg_message("Please select the raw data file containing all patients in your GP database.
              Continue by pressing OK",
    type = c("ok"), gui = .GUI
  )
  path_patients <- tcltk::tk_choose.files(caption = "Select patient table")

  if (umc != "rotterdam") {
    # Contact types
    svDialogs::dlg_message("Please select the list of eligible contact.
              Continue by pressing OK",
      type = c("ok"), gui = .GUI
    )


    path_contact.types <- tcltk::tk_choose.files(caption = "Select list of eligible contacts")
  }

  # Change column names: visits --------------------------------------
  if (umc %in% c("utrecht", "maastricht", "amsterdam", "groningen")) {
    columns_visits <- c(
      "pat", "contactid", "contactd",
      "cod", "Contactsoort"
    )
  } else if (umc == "rotterdam") {
    columns_visits <- c(
      "exppatidx", "contactid", "contactd", "string_agg"
    )
  }

  # Change column names: patients --------------------------------------
  if (umc %in% c("utrecht", "maastricht", "amsterdam", "groningen")) {
    columns_patients <- c(
      "pat", "gender", "patbird", "Inschrijfdatum", "Datum_laatste_extractie",
      "Praktijk_id", "Uitschrijfdatum"
    )
  } else if (umc == "rotterdam") {
    columns_patients <- c(
      "exppatidx", "gender", "patbird", "ipcistd", "ipciendd", "prakid"
    )
  }

  # .-------------------------------------------------------------------------
  # Load and format visits data ---------------------------------------------
  # .-------------------------------------------------------------------------

  # pat_id: patient's unique id

  # icpc: ICPC code recored in each GP consultation (five digits)

  # contact_date: date of the GP consultation in yyyy-mm-dd format

  # age: age in years of the patient on the date of the consultation,
  # rounded to two decimal points

  # month: month of visit
  # year: year of visit
  # monthyear: month and year of visit, stored as the 15th day of the month in which
  # the visit took place
  # week_start: date of the first day of the week in which the visit took place.
  # the first day of the week is Monday!

  # age_group: age group of the patient on the date of the visit. Age brackets are
  # defined as ten year interval starting from 0:
  # "<10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", ">80"

  visits <- readr::read_csv(file = path_visits, progress = FALSE, show_col_types = FALSE)

  visits <- visits %>%
    dplyr::rename(
      pat_id = columns_visits[1],
      contact_id = columns_visits[2],
      contact_date = columns_visits[3],
      icpc = columns_visits[4]
    )

  if (umc != "rotterdam") {
    consult.contact <- readxl::read_excel(path = path_contact.types)[, 1]
    names(consult.contact) <- "types"
    consult.contact <- consult.contact %>% stats::na.omit()

    visits <- visits %>%
      dplyr::rename(contact_type = columns_visits[5]) %>%
      # keep only contact types considered a consultations
      # (based on Utrecht definition).
      # The variable eligible_c is a boolean vector to indicate whether a certain contact
      # is considered a true contact
      mutate(eligible_c = if_else(contact_type %in% consult.contact$types, TRUE, FALSE))
  }

  # select only relevant columns
  visits <- visits %>%
    dplyr::select(pat_id, contact_id, contact_date, icpc) %>%
    # variable formatting
    dplyr::mutate(
      pat_id = factor(pat_id),
      contact_id = factor(contact_id),
      contact_date = anytime::anydate(contact_date),
      icpc = as.character(icpc),
      month = lubridate::month(contact_date),
      year = lubridate::year(contact_date),
      monthyear = lubridate::floor_date(contact_date, unit = "month") + 14,
      week_start = lubridate::floor_date(contact_date,
        unit = "week",
        week_start = getOption("lubridate.week.start", 1)
      )
    )


  # .-------------------------------------------------------------------------
  # Load and format patient data ---------------------------------------------
  # .-------------------------------------------------------------------------

  # pat_id: patient's unique id

  # fu_start: follow up start date in yyyy-mm-dd format,
  # typically date of registration with a GP practice

  # fu_end: follow up end date in yyyy-mm-dd format,
  # typically either date of de-registration with a GP practice
  # date of data collection end for patient's GP practice, date of death or end
  # of the study period, whichever comes first

  # fu: follow up time, as a time interval between fu_start and fu_end

  # sex: sex of the patient


  patients <- readr::read_csv(file = path_patients, progress = FALSE, show_col_types = FALSE)

  patients <- patients %>%
    dplyr::rename(
      pat_id = columns_patients[1],
      sex = columns_patients[2],
      pat_dob = columns_patients[3],
      fu_start = columns_patients[4],
      fu_end = columns_patients[5],
      prak_id = columns_patients[6]
    )

  if(umc != "rotterdam") {
    patients <- patients %>% dplyr::rename(dereg_date = columns_patients[7])
  }

  # format columns
  patients <- patients %>%
    dplyr::mutate(
      pat_id = factor(pat_id),
      sex = factor(sex),
      pat_dob = anytime::anydate(pat_dob),
      fu_start = anytime::anydate(fu_start),
      fu_end = anytime::anydate(fu_end),
      prak_id = factor(prak_id)
    )

  if (umc != "rotterdam") {
    patients <- patients %>%
      dplyr::mutate(dereg_date = anytime::anydate(dereg_date)) %>%

      # follow up end is either date of de-registration or date of the last extraction (whichever came first)
      dplyr::mutate(
        fu_end = pmin(fu_end, dereg_date, na.rm = TRUE),
        sex = factor(sex, levels = c("M", "V"), labels = c("Male", "Female"))
      ) %>%
      dplyr::select(-dereg_date)
  } else {
    patients <- patients %>%
      dplyr::mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))
  }

  patients <- patients %>%
    dplyr::select(pat_id, sex, pat_dob, fu_start, fu_end, prak_id) %>%

    # remove any patient with missing data for DOB or sex
    dplyr::filter(!is.na(pat_dob),
                  !is.na(sex))



  patients_to_join <- patients %>% dplyr::select(pat_id, sex, pat_dob, prak_id)

  # .-------------------------------------------------------------------------
  # Merge patient and visit data ---------------------------------------------
  # .-------------------------------------------------------------------------

  visits <- visits %>%
    dplyr::left_join(., patients_to_join, by = "pat_id") %>%

    # remove any patients from the visits table with missing data for DOB or sex
    dplyr::filter(!is.na(pat_dob),
                  !is.na(sex)) %>%

    dplyr::mutate(
      age = round(lubridate::interval(pat_dob, contact_date) / lubridate::years(1), 2),
      age_g = factor(
        cut(age,
          breaks = c(-Inf, 19, 39, 59, 79, Inf),
          labels = c("0-19", "20-39", "40-59", "60-79", "80+")
        ),
        levels = c("0-19", "20-39", "40-59", "60-79", "80+")
      )
    )


  patients <- patients %>%
    dplyr::mutate(

      # move the fu_end by one date for rotterdam data, as the fu_end is recorded
      # on the first day of the month following the true follow up end date
      fu_end = `if`(
        umc == "rotterdam",
        fu_end - lubridate::days(1), fu_end
      ),
      fu = lubridate::interval(fu_start, fu_end)
    )

  # remove any potential rows with all missing values
  visits <- visits %>% dplyr::filter(!dplyr::if_all(everything(), is.na))
  patients <- patients %>% dplyr::filter(!dplyr::if_all(everything(), is.na))

  # since rotterdam data include all visits, and not only cvd visits, we need to first
  # exclude all visits for non-cvd complaints
  if (umc == "rotterdam") {
    visits <- visits[grepl("K", visits$icpc, fixed = TRUE), ]
  }

  # .-------------------------------------------------------------------------
  # only include unique consultations  ---------------------------------------
  # .-------------------------------------------------------------------------

  # report on duplicate visits

  # multiple contacts by the same patients, same ICPC code and same contact id
  unique_visits_code_contact_id <- visits %>%
    dplyr::distinct(pat_id, contact_id, icpc) %>%
     nrow()

  # multiple contacts by the same patients, same ICPC code and same contact id and on the same day
  unique_visits_code_contact_id_date <- visits %>%
    dplyr::distinct(pat_id, contact_id, icpc, contact_date) %>%
    nrow()

  # multiple contacts by the same patients, same ICPC code and on the same day
  unique_visits_code_date <- visits %>%
    dplyr::distinct(pat_id, icpc, contact_date) %>%
    nrow()

  # multiple contacts by the same patients, on the same day
  unique_visits_date <- visits %>%
    dplyr::distinct(pat_id, contact_date) %>%
    nrow()

  duplicated <- data.frame(duplicated_contacts =
                             c("same patient and date",
                               "same patient, date and code",
                               "same patient, contact id and code",
                               "same patient, date, contact id, and code"),
                                      n = c(nrow(visits) - unique_visits_date,
                                            nrow(visits) - unique_visits_code_date,
                                            nrow(visits) - unique_visits_code_contact_id,
                                            nrow(visits) - unique_visits_code_contact_id_date)) %>%
    mutate(proportion = round(n / nrow(visits), 2))

  # And now only with contacts defined as consultations, only relevant for data outside rotterdam

  duplicated_eligible_c <- data.frame(NULL)
  if (umc != "rotterdam"){

    unique_visits_code_contact_id_eligible_c <- visits %>%
      dplyr::filter(eligible_c == TRUE) %>%
      dplyr::distinct(pat_id, contact_id, icpc) %>%
      nrow()

    unique_visits_code_contact_id_date_eligible_c <- visits %>%
      dplyr::filter(eligible_c == TRUE) %>%
      dplyr::distinct(pat_id, contact_id, icpc, contact_date) %>%
      nrow()

    unique_visits_code_date_eligible_c <- visits %>%
      dplyr::filter(eligible_c == TRUE) %>%
      dplyr::distinct(pat_id, icpc, contact_date) %>%
      nrow()

    duplicated_eligible_c <- data.frame(duplicated_contacts =
                                          c("same patient and date",
                                            "same patient, date and code",
                                            "same patient, contact id and code",
                                            "same patient, date, contact id, and code"),
                              n = c(nrow(visits) - unique_visits_date,
                                    nrow(visits) - unique_visits_code_date_eligible_c,
                                    nrow(visits) - unique_visits_code_contact_id_eligible_c,
                                    nrow(visits) - unique_visits_code_contact_id_date_eligible_c)) %>%
      mutate(proportion = round(n / nrow(visits), 2))
    }


  if(umc != "rotterdam"){
    report <- list(all_contacts = duplicated,
                   eligible_contacts = duplicated_eligible_c)
  } else {report <- list(all_contacts = duplicated)
    }


  # multiple contacts by the same patients, same ICPC code, same contact id and same contact date
  unique_visits <- visits %>%
    dplyr::distinct(pat_id, contact_id, icpc, contact_date) %>%
    nrow()

  difference <- round(((nrow(visits) - unique_visits) / nrow(visits)) * 100, 2)

  if (difference > 0) {
    warning(paste("Warning: unique consultations and all
                               consultations differ! This may mean the raw data
                               contain duplicate consultations! \n

                  There is", difference, " % contacts with the same patient id,
                  contact id, date and ICPC code. \n

                  See the table below for more details.
                  "))
  }


  duplicated_n <- nrow(visits) - (visits %>% dplyr::distinct(pat_id, contact_date) %>% nrow())
  duplicated_p <- (1 - round(visits %>% dplyr::distinct(pat_id, contact_date) %>% nrow() / nrow(visits), 2)) * 100


  visits <- visits %>% dplyr::distinct(pat_id, contact_date, .keep_all = TRUE)


  if (!dir.exists(file.path(fs::path_dir(path_visits), "results"))) {
    dir.create(file.path(fs::path_dir(path_visits), "results"), recursive = TRUE)
  }

  # paths to cleaned dataframes
  path_cleaned_visits <- file.path(fs::path_dir(path_visits), "results", "visits.RData")
  path_cleaned_patients <- file.path(fs::path_dir(path_patients), "results", "patients.RData")

  # save cleaned data
  saveRDS(visits, file = path_cleaned_visits, compress = FALSE)
  saveRDS(patients, file = path_cleaned_patients, compress = FALSE)

  message(paste(
    "Cleaning successful! \nCleaned dataframes have been saved as 'visits.RData' and
    'patients.RData' and are stored in the following folder:", file.path(fs::path_dir(path_visits), "results")
  ))

  cat(paste("There were ", duplicated_n, " (",duplicated_p,"%) of duplicated contacts, ie. patients with
            multiple GP contacts within the same day. Those were removed.
            See the table below for more details.", sep = ""))

  # return report on duplicated values in the data
  report
}
