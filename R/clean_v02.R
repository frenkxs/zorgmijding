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
#' @importFrom anytime anydate
#' @import lubridate
#' @import dplyr
#' @import vroom


#' @param umc University medical centre that provides the data. It can take the
#' the following values: "utrecht", "maastricht", "amsterdam", "groningen", "rotterdam"
#'
#' @param clean_types TRUE or FALSE value indicating whether the list of contact
#' types should be used for cleaning the data. If set to TRUE then only
#' eligible contact types will be considered, based on the 'Contactsoort' variable. You
#' can access the list of eligible contact types by data("consult_contact")
#' @param age_groups Definition of age groups stratification by age.
#'
#' @param col_visits Vector with the names of columns in the visits table in the order in which
#' they occur in the table. Use the term below for select variables (on the left hand side is the
#' original variable name, on the right hand side to one to be used):
#'
#' - pat: pat_id
#' - contactid: contact_id
#' - contactd: contact_date
#' - cod: ICPC
#'
#' @param col_pats Vector with the names of columns in the patients table in the order in which
#' they occur in the table. Use the term below for select variables (on the left hand side is the
#' original variable name, on the right hand side to one to be used):
#'
#' - pat: pat_id
#' - Gender: sex
#' - patbird: pat_dob
#' - Datum_laatste_extractie: fu_end
#' - Inschrijfdatum: fu_start
#' - Praktijk_id: prak_id
#' - Uischrijfdatum: dereg_date


#' @export
clean_data <- function(umc = c(
                             "utrecht", "maastricht", "amsterdam",
                             "groningen", "rotterdam"
                           ),
                           clean_types = FALSE,
                           age_groups = c("0-19", "20-39", "40-59", "60-79", "80+"),
                           col_visits = c(
                             "pat_id", "sex", "pat_dob", "prakid", "Hisnaam",
                             "contact_id", "contact_date", "Soepcode",
                             "Contactsoort", "icpc"
                           ),
                           col_pats = c(
                             "pat_id", "sex", "pat_dob", "fu_end", "fu_start",
                             "prak_id", "dereg_date"
                           )) {
  # Preliminaries ---------------------------------------
  umc <- match.arg(umc)

  # clean types set to TRUE with rotterdam data doesn't make sense as there are
  # no contact types in rotterdam data
  if (umc == "rotterdam" & clean_types == TRUE) {
    clean_types <- FALSE
    warning("Setting 'clean_types' to FALSE, as Rotterdam data doesn't work with
            contact types")
  }

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

  # Change column names: visits --------------------------------------
  if (umc %in% c("utrecht", "maastricht", "amsterdam", "groningen")) {
    columns_visits <- col_visits

    columns_visits_select <- c(
      "pat_id", "sex", "pat_dob", "contact_id", "contact_date",
      "Contactsoort", "icpc"
    )
  } else if (umc == "rotterdam") {
    columns_visits <- c(
      "prakid", "pat_dob", "patdead", "sex", "livenv", "socdep56",
      "pat_id", "contact_date", "contact_id", "icpc"
    )
    columns_visits_select <- c(
      "pat_id", "pat_dob", "sex", "contact_id", "contact_date", "icpc"
    )
  }


  # Change column names: patients --------------------------------------
  if (umc %in% c("utrecht", "maastricht", "amsterdam", "groningen")) {
    columns_patients <- col_pats
    columns_patients_select <- col_pats
  } else if (umc == "rotterdam") {
    columns_patients <- c(
      "prak_id", "pat_id", "pat_dob", "patdead", "sex",
      "livenv", "socdep56", "fu_start", "fu_end"
    )
    columns_patients_select <- c(
      "pat_id", "sex", "pat_dob", "fu_start", "fu_end", "prak_id"
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

  visits <- load_data(
    path = path_visits,
    column_names = columns_visits,
    column_select = columns_visits_select,
    with_contact_types = clean_types
  )


  visits <- visits %>%
    # variable formatting
    dplyr::mutate(
      pat_id = factor(pat_id),
      contact_id = factor(contact_id),
      contact_date = anytime::anydate(contact_date, tz = "CET"),
      pat_dob = anytime::anydate(pat_dob),
      sex = factor(sex),
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


  patients <- vroom::vroom(
    file = path_patients,
    col_names = columns_patients,
    col_select = columns_patients_select,
    skip = 1,
    col_types = vroom::cols(.default = "c")
  )

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
    dplyr::filter(
      !is.na(pat_dob),
      !is.na(sex)
    )




  # .-------------------------------------------------------------------------
  # Merge patient and visit data ---------------------------------------------
  # .-------------------------------------------------------------------------

  # get breaks from the user defined age groups
  br <- as.numeric(substring(age_groups, nchar(age_groups) - 1)[-length(age_groups)])

  visits <- visits %>%
    # Recode the sex variable. One set of values is for Intercity databases, the other for Rotterdam
    # data
    mutate(sex = recode(sex,
                        "1" = "Male", "2" = "Female",
                        "M" = "Male", "V" = "Female")) %>%

    # remove any patients from the visits table with missing data for DOB or sex
    dplyr::filter(
      !is.na(pat_dob),
      !is.na(sex)
    ) %>%

    dplyr::mutate(
      age = lubridate::interval(pat_dob, contact_date) / lubridate::years(1),
      age_g = factor(
        cut(age,
          breaks = c(-Inf, br, Inf),
          labels = age_groups
        ),
        levels = age_groups
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

  # create a directory to save the resulting data
  if (!dir.exists(file.path(fs::path_dir(path_visits), "results"))) {
    dir.create(file.path(fs::path_dir(path_visits), "results"), recursive = TRUE)
  }

  # paths to cleaned dataframes
  path_cleaned_visits <- file.path(fs::path_dir(path_visits), "results", "visits.RData")
  path_cleaned_patients <- file.path(fs::path_dir(path_patients), "results", "patients.RData")

  # save cleaned data
  saveRDS(visits, file = path_cleaned_visits, compress = FALSE)
  saveRDS(patients, file = path_cleaned_patients, compress = FALSE)

  message(strwrap(prefix = "\n", initial = "\n", paste(
    "Cleaning successful! \nCleaned dataframes have been saved as 'visits.RData' and
    'patients.RData' and are stored in the following folder:",
    file.path(fs::path_dir(path_visits), "results")
  )))
}



# custom function to load large data in batches
load_data <- function(path, column_names, column_select,
                      with_contact_types) {
  # size of the batches
  batch_size <- 1e6

  # number of iterations. it should be large enough to make sure we get the entire data
  n_iterations <- 50

  data("consult_contact")

  # read-in the first batch of data
  res <- vroom::vroom(path,
    col_names = column_names, col_select = column_select,
    skip = 1, n_max = batch_size,
    col_types = vroom::cols(.default = "c")
  ) %>%
      # record the number of original rows
      structure("n_rows_total" = nrow(.)) %>%
      # only consider cvd visits (ie ICPC code in K chapter)
      dplyr::filter(grepl("K[0-9]{2}", icpc)) %>%
      # record number of rows in the data after removing non-CVD contacts
      structure("n_rows_cvd" = nrow(.)) %>%
      # select only eligible contact types if with_contact_types is TRUE
      dplyr::filter(if (with_contact_types) {
        Contactsoort %in% consult_contact
      } else {
         TRUE
      }) %>%
      # remove duplicates within this first batch
      dplyr::distinct(pat_id, contact_date, .keep_all = TRUE)

  # check the number of rows in the first batch. If less than batch size,
  # then we have all the data and can return them
  if (attributes(res)$n_rows_total < batch_size) {
      n_rows_cvd <- attributes(res)$n_rows_cvd

  # if there the total number of row is not less than the batch size, we load the rest
  # in batches
      } else {

      # sequence of row numbers where we split the data into batches
      d <- seq(1, n_iterations * batch_size, by = batch_size)

      # set counter to keep track of the number of iterations
      counter <- 0

      # count the total number of CVD contacts (rows), both eligible and ineligible
      n_rows_cvd <- attributes(res)$n_rows_cvd

      # now we read-in the batches one by one in a for loop. Since we've already read
      # in the first batch, we can start from the second term in the sequence
      for (i in d[-1]) {
        from <- i
        batch <- vroom::vroom(path,
          col_names = column_names, col_select = column_select,
          skip = from, n_max = batch_size,
          show_col_types = FALSE,
          col_types = cols(.default = "c")
        ) %>%
          # record the number of original rows
          structure("n_rows_total" = nrow(.)) %>%
          # only consider cvd visits (ie ICPC code in K chapter)
          dplyr::filter(grepl("K[0-9]{2}", icpc)) %>%
          # record the number of cvd only rows
          structure("n_rows_cvd" = nrow(.))

        # count the number of iterations and run garbage collector every third iteration
        counter <- counter + 1
        if (counter %% 3) gc(verbose = FALSE)

        # keep track of all cvd rows across batches
        n_rows_cvd <- n_rows_cvd + attributes(batch)$n_rows_cvd

        #' check how many rows there are in the batch (before any filter are applied).
        #' If less than the batch size, then we are at the end of the data and we can
        #' step out of the loop at the end of this iteration. If it is completely empty,
        #' we've read all the data in the previous iteration and we can step out of the loop now.
        if (attributes(batch)$n_rows_total == 0) break

        # remove duplicates within the batch
        batch <- batch %>%
          # first select only eligible contact types if with_contact_types is TRUE
          dplyr::filter(if (with_contact_types) {
            Contactsoort %in% consult_contact
          } else {
            TRUE
          }) %>%
          dplyr::distinct(pat_id, contact_date, .keep_all = TRUE)

        res <- dplyr::bind_rows(res, dplyr::anti_join(batch, res,
          by = join_by("pat_id", "contact_date")
        ))

        if (attributes(batch)$n_rows_total < batch_size) break
      }
  }

  comment(res) <- strwrap(prefix = " ", initial = "", paste(
    "Originally there was ", n_rows_cvd, " rows. It was reduced to ", nrow(res),
    "(", round(nrow(res) / n_rows_cvd, 2) * 100, "%) after removing duplicates
    and ineligible contact types (if relevant).",
    sep = ""
  ))
  message(comment(res))
  res
}
