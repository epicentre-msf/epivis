
#' Plot Age/Sex Pyramids
#'
#' @param df un-aggregated dataframe with a minimum of age and gender variables.
#' @param age_col age variable name in `df`. Can be either a numeric vecotr of ages
#'  or a character/factor vector of age groups.
#' @param gender_col gender variable name in `df` with levels indicating male or female.
#' @param gender_levels length 2 character vector with male and female level in `gender_col`, respectively.
#' @param facet_col optional faceting variable name to split chart into small multiples.
#' @param make_age_groups set to TRUE (default) if `age_col` is numeric and needs to be binned into groups.
#' @param age_breaks breaks to be used for binning a numerical `age_col`.
#' @param age_labels break labels to accompany `age_breaks`. Defaults to `epivis::label_breaks(age_breaks)`.
#' @param drop_age_levels should age groups with no observations be removed from the chart? Defaults to FALSE.
#' @param gender_labs optional labels for `gender_levels`
#' @param x_lab optional label for the X axis.
#' @param y_lab optional label for the Y axis.
#' @param colours length 2 character vector of colours used for male and female, respectively.
#' @param show_data_labs show data labels on chart? Defaults to FALSE.
#' @param lab_size data labels size.
#' @param lab_in_col data label colour when placed inside a bar.
#' @param lab_out_col data label colour when placed outside a bar.
#' @param lab_nudge_factor threshold for moving a data label outside a bar. Defaults to 5.
#'  Increasing the number increases the distance from the max value required to move a label outside the bar.
#' @param facet_nrow nrow argument passed to [`ggplot2::facet_wrap`].
#' @param facet_ncol ncol argument passed to [`ggplot2::facet_wrap`].
#' @param facet_scales facet scales argument passed to [`ggplot2::facet_wrap`].
#'  Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param facet_labs facet labeller argument passed to [`ggplot2::facet_wrap`]. Defaults to [`ggplot2::label_wrap_gen(width = 25)`].
#' @param facet_lab_pos facet label position argument passed to strip.position in [`ggplot2::facet_wrap`].
#'  Defaults to "top". Options are `c("top", "bottom", "left", "right")`.
#' @param add_missing_cap show missing data counts for `age_col` and `gender_col`? Defaults to TRUE.
#'
#' @return a ggplot object
#'
#' @examples
#'
#' suppressMessages(library(dplyr))
#' df_flu <- outbreaks::fluH7N9_china_2013
#'
#' plot_pyramid(
#'   df = df_flu,
#'   age_col = age,
#'   gender_col = gender,
#'   gender_levels = c("m", "f")
#' )
#'
#' @export
#'
plot_pyramid <- function(
    df,
    age_col,
    gender_col,
    gender_levels,
    facet_col = NULL,
    make_age_groups = TRUE,
    age_breaks = c(seq(0, 80, 10), Inf),
    age_labels = label_breaks(age_breaks),
    drop_age_levels = FALSE,
    gender_labs = NULL,
    x_lab = waiver(),
    y_lab = waiver(),
    colours = c("#486090FF", "#7890A8FF"),
    show_data_labs = FALSE,
    lab_size = 4,
    lab_in_col = "white",
    lab_out_col = "grey30",
    lab_nudge_factor = 5,
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_scales = "fixed",
    facet_labs = label_wrap_gen(width = 25),
    facet_lab_pos = "top",
    add_missing_cap = TRUE
) {

  if (length(gender_levels) != 2) {
    stop("`gender_levels` should be of length 2 i.e. `c('m', 'f')`")
  }

  if (is.null(gender_labs)) {
    gender_labs <- gender_levels
  }

  if (length(gender_labs) != 2) {
    stop("`gender_labs` should be of length 2 i.e. `c('Male', 'Female')`")
  }

  missing_gender <- df |> filter(!({{gender_col}}) %in% gender_levels | is.na({{gender_col}})) |> nrow()

  if (make_age_groups) {
    df_plot <- df |> mutate(age_group = suppressWarnings(as.numeric(as.character({{age_col}}))))
    missing_age <- sum(is.na(df_plot$age_group))
    df_plot <- df_plot |>
      tidyr::drop_na(age_group) |>
      dplyr::mutate(age_group = cut(age_group, breaks = age_breaks, labels = age_labels, right = FALSE, include.lowest = TRUE))

  } else {
    if (!class(dplyr::pull(df, {{age_col}})) %in% c("factor", "character")) {
      stop("If not creating age groups with `make_age_groups = TRUE` from a numeric age vector, `age_col` should be a factor or a character vector")
    }

    #needs to define df_plot if make_age_groups = FALSE
    df_plot <- df |>
      tidyr::drop_na(age_group)

    missing_age <- sum(is.na(df_plot$age_group))

  }

  df_plot <- df_plot |> filter({{gender_col}} %in% gender_levels)

  missing_total <- nrow(df) - nrow(df_plot)

  g_vars <- dplyr::enquos(facet_col, gender_col)

  df_plot <- df_plot |>
    count(!!!g_vars, age_group) |>
    mutate(n = if_else({{gender_col}} == gender_levels[1], -n, n))

  p <- ggplot(df_plot, aes(x = age_group, y = n, fill = {{gender_col}})) +
    geom_col(colour = NA) +
    geom_hline(yintercept = 0, colour = "black") +
    coord_flip() +
    scale_x_discrete(drop = drop_age_levels) +
    scale_fill_manual(name = NULL, values = colours, breaks = gender_levels, labels = gender_labs) +
    scale_y_continuous(limits = pyramid_limits, breaks = pyramid_brks, labels = pyramid_labs) +
    theme(legend.position = "top") +
    labs(x = x_lab, y = y_lab)  +
    guides(y.sec = guide_axis_label_trans(~paste(.x))) # show age group labels on both axis

  if (!missing(facet_col)) {
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

  if (show_data_labs) {
    p <- p +
      geom_text(
        aes(label = scales::number(abs(n), accuracy = 1),
            hjust = pyramid_labs_pos(n, f = lab_nudge_factor),
            colour = pyramid_labs_colour(n, f = lab_nudge_factor, in_col = lab_in_col, out_col = lab_out_col)),
        size = lab_size,
        fontface = "bold"
      ) +
      scale_colour_identity()
  }

  if (add_missing_cap) {
    caption <- if_else(
      missing_total > 0,
      glue::glue("Missing data for {missing_total} observations (Age: {missing_age}, Gender: {missing_gender})"),
      "No missing data"
    )
    p <- p + labs(caption = caption)
  }

  return(p)
}

#' @noRd
pyramid_brks <- function(x, n = 3) {
  brks <- pretty(0:max(abs(x)), n = n)
  c(-brks, brks)
}

#' @noRd
pyramid_labs <- function(x) {
  #scales::label_number_si()(abs(x))
  scales::label_number()(abs(x))
}

#' @noRd
pyramid_limits <- function(x) {
  c(-max(abs(x)), max(abs(x)))
}

#' @noRd
pyramid_labs_pos <- function(x, f = 5) {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ -0.1,
    x > 0 & x > max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) < max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) > max(abs(x)) / f ~ -0.1
  )
}

#' @noRd
pyramid_labs_colour <- function(x, f = 5, in_col = "white", out_col = "grey30") {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ out_col,
    x > 0 & x > max(abs(x)) / f ~ in_col,
    x < 0 & abs(x) < max(abs(x)) / f ~ out_col,
    x < 0 & abs(x) > max(abs(x)) / f ~ in_col
  )
}

