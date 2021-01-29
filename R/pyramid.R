
#' Plot Age/Sex Pyramids
#'
#' @param df un-aggregated dataframe with a minimum of age and gender variables
#' @param age_col
#' @param gender_col
#' @param gender_levels
#' @param facet_col
#' @param make_age_groups
#' @param age_breaks
#' @param age_labels
#' @param drop_age_levels
#' @param gender_labs
#' @param x_lab
#' @param y_lab
#' @param colours
#' @param show_data_labs
#' @param lab_size
#' @param lab_in_col
#' @param lab_out_col
#' @param lab_nudge_factor
#' @param facet_col
#' @param facet_nrow
#' @param facet_ncol
#' @param facet_scales
#' @param facet_labs
#' @param facet_lab_pos
#' @param add_missing_cap
#'
#' @return a ggplot object
#'
#' @example
#'
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

  missing_gender <- df %>% filter(!({{gender_col}}) %in% gender_levels | is.na({{gender_col}})) %>% nrow()

  if (make_age_groups) {
    df_plot <- df %>% mutate(age_group = suppressWarnings(as.numeric(as.character({{age_col}}))))
    missing_age <- sum(is.na(df_plot$age_group))
    df_plot %<>%
      tidyr::drop_na(age_group) %>%
      mutate(age_group = cut(age_group, breaks = age_breaks, labels = age_labels, right = FALSE, include.lowest = TRUE))

  } else {
    if (!class(dplyr::pull(df, {{age_col}})) %in% c("factor", "character")) {
      stop("If not creating age groups with `make_age_groups = TRUE` from a numeric age vector, `age_col` should be a factor or a character vector")
    }
  }

  df_plot %<>% filter({{gender_col}} %in% gender_levels)

  missing_total <- nrow(df) - nrow(df_plot)

  g_vars <- dplyr::enquos(facet_col, gender_col)

  df_plot %<>%
    count(!!!g_vars, age_group) %>%
    mutate(n = if_else({{gender_col}} == gender_levels[1], -n, n))

  p <- ggplot(df_plot, aes(x = age_group, y = n, fill = {{gender_col}})) +
    geom_col(colour = NA) +
    geom_hline(yintercept = 0, colour = "black") +
    coord_flip() +
    scale_x_discrete(drop = drop_age_levels) +
    scale_fill_manual(name = NULL, values = colours, breaks = gender_levels, labels = gender_labs) +
    scale_y_continuous(limits = pyramid_limits, breaks = pyramid_brks, label = pyramid_labs) +
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

pyramid_brks <- function(x, n = 3) {
  brks <- pretty(0:max(abs(x)), n = n)
  c(-brks, brks)
}

pyramid_labs <- function(x) {
  scales::label_number_si()(abs(x))
}

pyramid_limits <- function(x) {
  c(-max(abs(x)), max(abs(x)))
}

pyramid_labs_pos <- function(x, f = 5) {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ -0.1,
    x > 0 & x > max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) < max(abs(x)) / f ~ 1.1,
    x < 0 & abs(x) > max(abs(x)) / f ~ -0.1
  )
}

pyramid_labs_colour <- function(x, f = 5, in_col = "white", out_col = "grey30") {
  dplyr::case_when(
    x > 0 & x < max(abs(x)) / f ~ out_col,
    x > 0 & x > max(abs(x)) / f ~ in_col,
    x < 0 & abs(x) < max(abs(x)) / f ~ out_col,
    x < 0 & abs(x) > max(abs(x)) / f ~ in_col
  )
}
