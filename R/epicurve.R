
#' Plot incidence over time from patient level data
#'
#' Helper function to plot epidemic curves with ggplot2 with options for
#' grouping data, facets and proportion lines.
#'
#' @param df unaggregated dataframe of with a minumum of a date column with a date or POSIX class
#' @param date_col date variable to plot incidence with. Must be provided.
#' @param group_col optional grouping variable to be applied to the fill aesthetic of columns
#' @param facet_col optional faceting variable to split chart into small multiples
#' @param prop_col optional variable to be used to plot a proportion line on top of the epicurve
#' @param prop_numer value(s) in the `prop_col` variable as a single value or vector to be used to calculate the numerator of the proportion calculation
#' @param prop_denom value(s) in the `prop_col` variable as a single value or vector to be used to calculate the denominator of the proportion calculation.
#'  default "non_missing" will take the sum of all non-missing values in the column.
#' @param prop_line_colour colour of the proportion line. defaults to "red"
#' @param prop_line_size width of the proportion line. defaults to 0.8
#' @param floor_date_week should `date_col` dates be floored to the Monday of the ISO week they fall in? defaults to TRUE
#' @param label_weeks label primary date axis with week numbers? defaults to TRUE
#' @param week_start day of week defined as the start of the week. defaults to "Monday" (ISO week standard)
#' @param date_breaks date break intervals passed to [`ggplot2::scale_x_date`]. defaults to "2 weeks"
#' @param date_labels [`base::strptime`] date label code passed to [`ggplot2::scale_x_date`]. defaults to "\\%V" (ISO Week)
#' @param sec_date_axis plot a secondary date axis using default calculated ggplot2 date breaks and labels? defaults to FALSE
#' @param facet_nrow nrow argument passed to [`ggplot2::facet_wrap`]
#' @param facet_ncol ncol argument passed to [`ggplot2::facet_wrap`]
#' @param facet_labs facet labeller argument passed to [`ggplot2::facet_wrap`]. Defaults to [`label_wrap_gen(width = 25)`]
#' @param facet_lab_pos facet label position argument passed to strip.position in [`ggplot2::facet_wrap`]. defaults to "top". Options are `c("top", "bottom", "left", "right")`
#' @param group_na_colour colour for missing values in `group_col`. defaults to "grey"
#' @param title optional title for the plot
#' @param subtitle optional subtitle for the plot
#' @param date_lab optional label for the date axis. defaults to `date_col` name if not provided
#' @param y_lab optional label for the Y axis. defaults to `n` if not provided
#' @param group_lab optional label for the group legend. defaults to `group_col` name if not provided
#' @param prop_lab label for the proportion line. There is no default so this should be provided when plotting proportion lines
#'
#' @return a ggplot object
#'
#' @examples
#'
#' df_ebola <- dplyr::as_tibble(outbreaks::ebola_sim_clean$linelist)
#'
#' df_ebola %>%
#'   dplyr::mutate(outcome = forcats::fct_explicit_na(outcome, "Unknown")) %>%
#'   plot_epicurve(
#'     date_col = date_of_onset,
#'     group_col = outcome,
#'     prop_col = outcome,
#'     prop_numer = "Death",
#'     prop_denom = c("Death", "Recover"),
#'     floor_date_week = TRUE,
#'     date_breaks = "2 weeks",
#'     sec_date_axis = TRUE,
#'     date_lab = "Week of onset",
#'     y_lab = "Incidence",
#'     group_lab = "Outcome",
#'     prop_lab = "CFR"
#'   )
#'
#' @import ggplot2
#' @export
plot_epicurve <- function(df,
                          date_col,
                          group_col = NULL,
                          facet_col = NULL,
                          prop_col = NULL,
                          prop_numer = NULL,
                          prop_denom = "non_missing",
                          prop_line_colour = "black",
                          prop_line_size = 0.8,
                          floor_date_week = FALSE,
                          label_weeks = FALSE,
                          week_start = "Monday",
                          date_breaks = "2 weeks",
                          date_labels = "%V",
                          sec_date_axis = FALSE,
                          facet_nrow = NULL,
                          facet_ncol = NULL,
                          facet_scales = "fixed",
                          facet_labs = label_wrap_gen(width = 25),
                          facet_lab_pos = "top",
                          group_na_colour = "grey",
                          title = waiver(),
                          subtitle = waiver(),
                          date_lab = waiver(),
                          y_lab = waiver(),
                          group_lab = waiver(),
                          prop_lab = NULL) {

  g_vars_1 <- dplyr::enquos(facet_col, date_col, group_col)
  g_vars_2 <- dplyr::enquos(facet_col, date_col)

  if(floor_date_week) {
    week_start <- match.arg(week_start, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), several.ok = FALSE)
    week_start <- switch (week_start,
                          Monday = 1,
                          Tuesday = 2,
                          Wednesday = 3,
                          Thursday = 4,
                          Friday = 5,
                          Saturday = 6,
                          Sunday = 7)
    df <- df %>% dplyr::mutate({{date_col}} := floor_week({{date_col}}, week_start = week_start))
    #df <- df %>% dplyr::mutate({{date_col}} := aweek::date2week({{date_col}}, week_start = week_start))
  }

  df_epicurve <- df %>%
    dplyr::count(!!!g_vars_1) #%>%
  #tidyr::drop_na()

  if (!missing(prop_col)) {
    if (is.null(prop_numer) | is.null(prop_denom))
      stop("Proportion numerator and demoninator values must be supplied")
    df_prop <- df %>%
      dplyr::select(!!!g_vars_1) %>%
      #tidyr::drop_na() %>%
      dplyr::group_by(!!!g_vars_2) %>%
      dplyr::summarise(
        n = dplyr::n(),
        denom = ifelse(prop_denom == "non_missing", sum(!is.na({{prop_col}})), sum({{prop_col}} %in% prop_denom)),
        num = sum({{prop_col}} %in% prop_numer, na.rm = TRUE),
        prop = num / denom,
        .groups = "drop"
      )

    n_max <- max(df_prop$n, na.rm = TRUE)
    prop_max <- rounder(max(df_prop$prop, na.rm = TRUE), .1)
    prop_max <- ifelse(prop_max > 1, 1, prop_max)
    scaling_factor <- prop_max / n_max # sets the y limit of proportion axis rounded up to nearest 10% above max
  }

  missing_dates <- sum(is.na(dplyr::pull(df, {{date_col}})))
  if(missing_dates > 0) {
    caption <- glue::glue("Missing Dates: {missing_dates}")
  } else {
    caption <- waiver()
  }

  p <- ggplot(df_epicurve, aes({{date_col}}, n))

  if(missing(group_col)) {
    p <- p + geom_col(fill = "steelblue", colour = "white", size = 0.2)
  } else {
    p <- p + geom_col(aes(fill = {{group_col}}), colour = "white", size = 0.2)
  }

  if(label_weeks) {
    date_vec <- df %>% tidyr::drop_na({{date_col}}) %>% dplyr::arrange({{date_col}}) %>% dplyr::pull({{date_col}})
    x_breaks <- seq.Date(date_vec[1], date_vec[length(date_vec)], by = date_breaks)
    x_labs <- pad_number(aweek::date2week(x_breaks, week_start = week_start, numeric = TRUE))
  } else {
    x_breaks <- waiver()
    x_labs <- waiver()
  }

  if(sec_date_axis) {
    p <- p + scale_x_date(breaks = x_breaks, labels = x_labs, sec.axis = ggplot2::sec_axis(trans = ~ .), expand = expansion(mult = c(0.01, 0.01)))
  } else {
    p <- p + scale_x_date(breaks = x_breaks, labels = x_labs, expand = expansion(mult = c(0.01, 0.01)))
  }

  # if(sec_date_axis) {
  #   p <- p + scale_x_date(date_breaks = date_breaks, date_labels = date_labels, sec.axis = ggplot2::sec_axis(trans = ~ .), expand = expansion(mult = c(0.01, 0.01)))
  # } else {
  #   p <- p + scale_x_date(date_breaks = date_breaks, date_labels = date_labels, expand = expansion(mult = c(0.01, 0.01)))
  # }

  if(!missing(facet_col)) {
    facet_lab_pos <- match.arg(facet_lab_pos, c("top", "bottom", "left", "right"))
    p <- p + facet_wrap(
      vars({{facet_col}}),
      nrow = facet_nrow,
      ncol = facet_ncol,
      scales = facet_scales,
      strip.position = facet_lab_pos,
      labeller = facet_labs
    )
  }

  if(!missing(prop_col)) {
    p <- p +
      geom_line(data = df_prop, aes(y = prop / scaling_factor, colour = prop_line_colour), key_glyph = "timeseries", size = prop_line_size) +
      scale_colour_identity(name = NULL, breaks = prop_line_colour, labels = paste(prop_lab, "%"), guide = "legend") +
      scale_y_continuous(breaks = integer_breaks(), labels = scales::number_format(accuracy = 1), expand = expansion(mult = c(0, 0.05)),
                         sec.axis = ggplot2::sec_axis(~ . * scaling_factor, name = prop_lab, labels = scales::percent_format(accuracy = 1)))
  } else {
    p <- p +
      scale_y_continuous(breaks = integer_breaks(), labels = scales::number_format(accuracy = 1), expand = expansion(mult = c(0, 0.05)))
  }

  p <- p +
    ggthemes::scale_fill_tableau(palette = "Tableau 10", na.value = group_na_colour) +
    labs(x = date_lab, y = y_lab, fill = group_lab, caption = caption, title = title, subtitle = subtitle) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 0)) +
    #theme_classic() +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    )

  return(p)
}
