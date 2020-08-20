
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' Re-set date to the first day of the week the date falls in
#'
#' @param date a date or something coercible by lubridate::as_date to a date
#' @param week_start first day of the week to re-set to. 7 = Sunday, 1 = Monday.
#'
#' @export
floor_week <- function(date, week_start = 1) {
  lubridate::floor_date(lubridate::as_date(date), unit = "week", week_start = week_start)
}

#' round number up or down to nearest 01, 100, 1000, etc
#'
#' @param x numeric vector
#' @param y 10 will round up to nearest 10, -10 down to nearest 10 etc
#'
#' @export
rounder <- function(x, y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
}

#' @export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#' @export
pyramid_brks <- function(x, n = 5) {
  brks <- pretty(0:max(abs(x)), n = n)
  c(-brks, brks)
}

#' @export
pyramid_limits <- function(x) {
  c(-max(abs(x)), max(abs(x)))
}

#' @export
pyramid_labs_pos <- function(x, f = 5) {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ -0.1,
    x > 0 & x > max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) < max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) > max(abs(x)) / f ~ -0.1
  )
}

#' @export
pyramid_labs_colour <- function(x, f = 5, in_col = "white", out_col = "grey30") {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ out_col,
    x > 0 & x > max(abs(x)) / f ~ in_col,
    x < 0 & abs(x) < max(abs(x)) / f ~ out_col,
    x < 0 & abs(x) > max(abs(x)) / f ~ in_col
  )
}

#' @param n.dodge
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
#' @export
add_breaks <- function(x, n = 5, style = "jenks", lab_accuracy = 1, replace_Inf = TRUE) {
  style <- match.arg(style, c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails"), several.ok = FALSE)
  breaks <- classInt::classIntervals(x, n = n, style = style)
  br <- breaks$brks
  cut(x, br, include.lowest = TRUE, right = FALSE, labels = label_breaks(br, lab_accuracy, replace_Inf))
}

#' @param breaks
#'
#' @param lab_accuracy accuracy of labels, passed to [`scales::label_number_si`]
#' @param replace_Inf if `Inf` is your final break, replace with a + sign in the label?
#'
#' @export
label_breaks <- function(breaks, lab_accuracy = 1, replace_Inf = TRUE) {
  labs <- sprintf(
    "%s-%s",
    frmt_num(breaks[1:length(breaks) - 1], accuracy = lab_accuracy),
    frmt_num(breaks[2:length(breaks)] - 1, accuracy = lab_accuracy)
  )
  if(replace_Inf){
    labs <- gsub("-Inf", "+", labs)
  }
  return(labs)
}

#' @param x a number to format
#'
#' @param accuracy accuracy of labels, passed to [`scales::label_number_si`]
#'
#' @export
frmt_num <- function(x, accuracy = 1) {
  scales::label_number_si(accuracy = accuracy)(x)
}

#' Duplicate discrete axis labels
#'
#' @param label_trans
#' @param ...
#'
#' @export
guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- ggplot2::guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}

#' Get max and second max values of a vector
#'
#' @param x
#'
#' @export
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
#' @export
get_prev_sunday <- function(date) {
  lubridate::floor_date(as.Date(date), unit = "week", week_start = 7)
}

#' @export
pad_number <- function(x) {
  formatC(as.numeric(x), width = 2, format = "d", flag = "0")
}
