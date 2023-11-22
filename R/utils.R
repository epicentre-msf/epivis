
#' @importFrom magrittr %>%
#' @noRd
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @noRd
magrittr::`%<>%`

#' Re-set date to the first day of the week the date falls in
#'
#' @param date a date or something coercible by lubridate::as_date to a date
#' @param week_start first day of the week to re-set to. 7 = Sunday, 1 = Monday.
#'
#' @noRd
floor_week <- function(date, week_start = 1) {
  lubridate::floor_date(lubridate::as_date(date), unit = "week", week_start = week_start)
}

#' round number up or down to nearest 01, 100, 1000, etc
#'
#' @param x numeric vector
#' @param y 10 will round up to nearest 10, -10 down to nearest 10 etc
#'
#' @noRd
rounder <- function(x, y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
}

#' @noRd
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#' Wrapper function to dodge xAxis labels
#'
#' Useful when you have overlapping labels on the xAxis.
#'
#' @param n.dodge passed to [ggplot2::guide_axis]
#'
#' @export
dodge_x_labs <- function(n.dodge = 2) {
  ggplot2::guides(x = ggplot2::guide_axis(n.dodge = n.dodge))
}

#' Bin numeric vector into breaks using [`classInt::classIntervals`]
#'
#' Works with [`dplyr::mutate`] to add a column of breaks. Breaks labels are formatted with [`scales::label_number_si`]
#'
#' @param x numeric vector
#' @param n number of bins
#' @param style of class intervals. see [`classInt::classIntervals`] for details
#' @param lab_accuracy accuracy of labels, passed to [`scales::label_number_si`]
#' @param replace_Inf if `Inf` is your final break, replace with a + sign in the label?
#'
#' @noRd
add_breaks <- function(x, n = 5, style = "jenks", lab_accuracy = 1, replace_Inf = TRUE) {
  style <- match.arg(style, c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails"), several.ok = FALSE)
  breaks <- classInt::classIntervals(x, n = n, style = style)
  br <- breaks$brks
  cut(x, br, include.lowest = TRUE, right = FALSE, labels = label_breaks(br, lab_accuracy, replace_Inf))
}

#' Format break labels
#'
#' @param breaks numeric vector of breaks
#' @param lab_accuracy accuracy of labels, passed to [`scales::number`]
#' @param replace_Inf if `Inf` is your final break, replace with a + sign in the label?
#'
#' @export
label_breaks <- function(breaks, lab_accuracy = .1, replace_Inf = TRUE) {
  labs <- sprintf(
    "%s-%s",
    frmt_num(breaks[1:length(breaks) - 1], accuracy = lab_accuracy),
    frmt_num(breaks[2:length(breaks)] - 1, accuracy = lab_accuracy)
  )
  if (replace_Inf) {
    labs <- gsub("-Inf", "+", labs)
  }
  return(labs)
}

#' Format numbers is units when large
#'
#' @param x a number to format
#' @param accuracy accuracy of labels, passed to [`scales::label_number_si`]
#'
#' @noRd
frmt_num <- function(x, accuracy = .1) {
  n <- scales::number(x, accuracy = accuracy, scale_cut = scales::cut_short_scale())
  n <- stringr::str_remove(n, "\\.0+(?=[a-zA-Z])")
  n <- stringr::str_remove(n, "\\.0+$")
  n
}

#' Format week labels for a chart axis
#'
#' Year labels will be displayed under the week label only for the first label
#' and for the first week of any new years that appear in the week breaks
#'
#' @param weeks character vector of dates to make week labels from
#' @param week_start day of week defined as the start of the week as integer 1-7 (Monday = 1, Sunday = 7). defaults to 1 (ISO week standard)
#' @param french if TRUE labels week as 'S' for Semaine rather than 'W' for Week
#' @param sep separator between week number and year for axis labels. defaults to "\n" (new line)
#'
#' @return character vector of week labels
#' @noRd
label_weeks <- function(weeks, week_start = 1, sep = "\n") {
  week_labs <- as.character(aweek::date2week(weeks, week_start = week_start, floor_day = TRUE))
  new_labs <- week_labs
  new_labs[1] <- paste(stringr::str_sub(week_labs[1], start = -3), stringr::str_sub(week_labs[1], 1, 4), sep = "\n")
  for (i in 2:length(week_labs)) {
    week_year <- stringr::str_sub(week_labs[i], 1, 4)
    prev_week_year <- stringr::str_sub(week_labs[i-1], 1, 4)
    if (week_year == prev_week_year) {
      new_labs[i] <- stringr::str_remove(week_labs[i], "\\d{4}-W")
    } else {
      new_labs[i] <- paste(stringr::str_sub(week_labs[i], start = -3), week_year, sep = sep)
    }
  }
  if (stringr::str_detect(Sys.getlocale("LC_TIME"), "FR|French")) new_labs <- stringr::str_replace(new_labs, "W", "S")
  return(new_labs)
}

#' Duplicate discrete axis labels
#'
#' @param label_trans label trans
#' @param ... additional arguments passed to [ggplot2::guide_axis]
#'
#' @noRd
guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- ggplot2::guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

#' @noRd
guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}

#' Get max and second max values of a vector
#'
#' @param x
#'
#' @noRd
max_2 <- function(x) {
  x <- unique(x) %>% purrr::discard(is.na) %>% sort()
  n <- length(x)
  c(dplyr::nth(x, n-1), dplyr::nth(x, n))
}

#' Get the date of the most recent Sunday
#'
#' Useful for filtering data to latest full ISO week
#'
#' @param date
#'
#' @noRd
get_prev_sunday <- function(date) {
  lubridate::floor_date(as.Date(date), unit = "week", week_start = 7)
}

#' @noRd
pad_number <- function(x) {
  formatC(as.numeric(x), width = 2, format = "d", flag = "0")
}
