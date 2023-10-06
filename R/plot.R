# .-----------------------------------------------------------------------------
# ---------------------- Zorgmijding project: plotting ------------------------
# .-----------------------------------------------------------------------------


# For all questions contact Premysl Velek at p.velek@erasmusmc.nl


#' plot_visits
#'
#' Plots the visit counts data (output of the n_visit function)

#' @importFrom svDialogs dlg_message
#' @importFrom tcltk tk_choose.files
#' @importFrom clock date_parse
#' @importFrom magrittr %T>%
#' @import colorspace
#' @import lubridate
#' @import ggplot2
#' @import dplyr


#' @param title Title of the plot
#' @param stratum How should the data be stratified, options are: 'sex': by sex, 'age': by age,
#' 'sex_age': by age and sex, and "total": across all strata (the default).
#'
#' Plot by sex will be shown on one panel, plots by age will be faceted across multiple panels

#' @param periodicity The period of counts, can either be 'd': day, 'w': week or 'm':
#' month (the default)
#' @param segment Time period to be plotted. By default, the data are plotted for the entire year,
#' with the pre-pandemic averages plotted against the 2020 data. However, when  plotting
#' daily data, it is recommended to use shorter period, which can be specified with
#' this argument. It takes a vector with two dates in the ymd format,
#' eg: c("2020-02-20", "2020-04-20").
#' @param show_40plus Specify whether you want to plot data for the entire population
#' or only for patients of 40 years and older
#' @param path Specify path to the  data to be plotted. Optional, if not provided
#' users will be prompted to select the data
#' @param point Size of the points on the resulting plot, defaults to 3
#' @param line Size of the points on the resulting plot, defaults to 1



#' @export
plot_visits <- function(title = "This is a title",
                        stratum = c("total", "age", "sex", "sex_age"),
                        periodicity = c("m", "w", "d"),
                        segment = "full",
                        show_40plus = TRUE,
                        path = NULL,
                        point = 3,
                        line = 1) {
  stratum <- match.arg(stratum)
  periodicity <- match.arg(periodicity)

  Sys.setlocale("LC_TIME", "C")

  if (is.null(path)) {
    svDialogs::dlg_message("Please select the data you want to plot. Continue by pressing OK.",
      type = c("ok"), gui = .GUI
    )
    path_results <- tcltk::tk_choose.files(caption = "Select results data")
  } else {
    path_results <- path
  }

  e <- new.env()
  load(path_results, envir = e)

  if (show_40plus) {
    if (grepl("age", stratum)) {
      res <- e[[paste("n_visits_", periodicity, "_", stratum, sep = "")]] %>%
        dplyr::filter(!(age_g %in% c("0-19", "20-39")))
    } else {
      stratum <- paste(stratum, "_40", sep = "")
      res <- e[[paste("n_visits_", periodicity, "_", stratum, sep = "")]]
    }
  } else {
    res <- e[[paste("n_visits_", periodicity, "_", stratum, sep = "")]]
  }

  res <- res %>%
    dplyr::ungroup() %T>%
    {
      options(warn = -1)
    } %>%
    dplyr::mutate(year_date = dplyr::case_when(
      periodicity == "m" ~ clock::date_parse(paste("2020-", year_date, "-15", sep = ""),
        format = "%Y-%B-%d"
      ),
      periodicity == "w" ~ clock::date_parse(paste("2020-", year_date, "-Mon", sep = ""),
        format = "%G-%V-%a"
      ),
      TRUE ~ clock::date_parse(paste("2020", year_date),
        format = "%G %V %a"
      )
    )) %T>% {
      options(warn = 0)
    }

  # we want to remove any dates later than 2020
  temp_max <- res$year_date[lubridate::year(res$year_date) < 2021]


  if (segment[1] != "full") {
    seg_limits <- lubridate::ymd(segment)
  } else if (segment[1] == "full" & periodicity == "w") {
    # since the first and last weeks are not directly comparable due to different lengths
    # in different years, they will not be plotted. In this case we select the second
    # minimum and maximum value among the available dates

    seg_limits <- c(
      min(res$year_date[res$year_date != min(res$year_date)]),
      max(temp_max[temp_max != max(temp_max)])
    )
  } else {
    seg_limits <- c(min(res$year_date), max(temp_max))
  }

  x_breaks <- dplyr::case_when(
    periodicity == "m" ~ c("1 month", "%b"),
    periodicity == "w" ~ c("2 weeks", "%d %b"),
    periodicity == "d" ~ c("week", "%d %b")
  )


  p_base <- ggplot2::ggplot(res, aes(x = year_date, y = rate)) +
    ggplot2::geom_blank() +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_date(
      limits = seg_limits, breaks = x_breaks[1], date_labels = x_breaks[2],
      name = NULL
    ) +
    ggplot2::ylim(0, NA)

  formats <- list(
    ggplot2::theme_minimal(base_size = 16),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)),
    colorspace::scale_color_discrete_sequential(palette = "Red-Blue", name = "")
  )

  line_size <- 1
  point_size <- 3

  if (stratum == "total" | stratum == "total_40") {
    add_on <- list(
      ggplot2::geom_line(ggplot2::aes(color = prepan, group = prepan), linewidth = line_size),
      ggplot2::geom_point(ggplot2::aes(color = prepan, group = prepan), size = point_size)
    )
  } else if (stratum == "age") {
    add_on <- list(
      ggplot2::geom_line(ggplot2::aes(color = prepan, group = prepan), linewidth = line_size),
      ggplot2::geom_point(ggplot2::aes(color = prepan, group = prepan), size = point_size),
      ggplot2::facet_wrap(~ age_g, ncol = 3)
    )
  } else if (stratum == "sex" | stratum == "sex_40") {
    add_on <- list(
      ggplot2::geom_line(ggplot2::aes(color = prepan, group = prepan), linewidth = line_size),
      ggplot2::geom_point(ggplot2::aes(color = prepan, group = prepan), size = point_size),
      ggplot2::facet_wrap(~ sex, ncol = 2)
    )
  } else {
    add_on <- list(
      ggplot2::geom_line(ggplot2::aes(color = prepan, group = prepan), linewidth = line_size),
      ggplot2::geom_point(ggplot2::aes(color = prepan, group = prepan), size = point_size),
      ggplot2::facet_grid(rows = vars(sex), cols = vars(age_g))
    )
  }

  suppressWarnings(print(p_base + add_on + formats))
}
