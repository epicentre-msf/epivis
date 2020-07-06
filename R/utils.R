
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
set_epi_week <- function(date, week_start = 1) {
  lubridate::floor_date(lubridate::as_date(date), unit = "week", week_start = week_start)
}

#' @export
rounder <- function(x,y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
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
dodge_x_labs <- function() {
  ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
}

#' Bin numeric vector into breaks using classInt::classIntervals
#'
#' Works with dplyr::mutate to add a column of breaks
#'
#' Breaks labels are formatted with scales::label_number_si
#'
#' @param x numeric vector
#' @param n number of bins
#' @param style of class intervals. see [`classInt::classIntervals`] for details
#' @export
add_breaks <- function(x, n = 5, style = "jenks", lab_accuracy = 1, replace_Inf = TRUE) {
  style <- match.arg(style, c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails"), several.ok = FALSE)
  breaks <- classInt::classIntervals(x, n = n, style = style)
  br <- breaks$brks
  cut(x, br, include.lowest = TRUE, labels = label_breaks(br, lab_accuracy, replace_Inf))
}

#' @export
label_breaks <- function(breaks, lab_accuracy = 1, replace_Inf = TRUE) {
  labs <- sprintf("%s-%s", frmt_num(breaks[1:length(breaks) - 1], accuracy = lab_accuracy), frmt_num(breaks[2:length(breaks)], accuracy = lab_accuracy))
  if(replace_Inf){
    labs <- gsub("-Inf", "+", labs)
  }
  return(labs)
}

#' @export
frmt_num <- function(x, accuracy = 1) {
  scales::label_number_si(accuracy = accuracy)(x)
}

#' Duplicate discrete axis labels
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

