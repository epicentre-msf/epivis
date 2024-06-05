#' Missing data visualisation
#'
#' @param x a dataframe
#' @param facet a character value of variable to facet the graph
#' @param col_vec a vector of length 2 specifying the color for Missing and Present values respectively
#' @param y_axis_text_size a nuemric value for the size of the y axis text
#'
#' @return a ggplot tile graph displaying the missing/present values for all variables of the dataframe
#' @export
#'
#' @examples
#' Use simulated measles data
#'
#' suppressMessages(library(dplyr))
#'
#' epivis::moissala_measles |>
#'   filter(site %in% c("Moïssala Hospital", "Bouna Hospital")) |>
#'   plot_miss_vis(facet = "site")

plot_miss_vis <- function(
  x,
  facet = NULL,
  col_vec = c("#6a040f", "#cce3de"),
  y_axis_text_size = 8
) {
  if (is.null(facet)) {
    tile_dat <- x |>
      dplyr::mutate(across(everything(), ~ case_when(
        is.na(.x) ~ "Missing",
        !is.na(.x) ~ "Present"
      ))) |>
      dplyr::mutate(
        observations = row_number()
      ) |>
      dplyr::relocate(observations, .before = 1) |>
      tidyr::pivot_longer(
        cols = !observations,
        names_to = "variable_name",
        values_to = "status"
      ) |>
      dplyr::mutate(
        .by = observations,
        pct_missing = sum(status == "Missing") / n()
      ) |>
      dplyr::mutate(
        order = as.numeric(forcats::fct_reorder(as.factor(observations), pct_missing, .desc = TRUE))
      )
  } else {
    tile_dat <- x |>
      dplyr::mutate(across(!.data[[facet]], ~ dplyr::case_when(
        is.na(.x) ~ "Missing",
        !is.na(.x) ~ "Present"
      ))) |>
      dplyr::mutate(
        .by = .data[[facet]],
        observations = row_number()
      ) |>
      dplyr::relocate(observations, .before = 1) |>
      tidyr::pivot_longer(
        cols = !observations & !.data[[facet]],
        names_to = "variable_name",
        values_to = "status"
      ) |>
      dplyr::mutate(
        .by = observations,
        pct_missing = sum(status == "Missing") / n()
      ) |>
      dplyr::mutate(
        .by = .data[[facet]],
        order = as.numeric(forcats::fct_reorder(as.factor(observations), pct_missing, .desc = TRUE))
      )
  }

  pct_df <- tile_dat |>
    dplyr::count(status) |>
    dplyr::mutate(pct = round(digits = 1, n / sum(n) * 100))

  pct_var_df <- tile_dat |>
    dplyr::count(variable_name, status) |>
    dplyr::mutate(
      .by = variable_name,
      pct = dplyr::if_else(status == "Present", 100 - (n / sum(n) * 100), n / sum(n) * 100)
    ) |>
    dplyr::filter(status == "Missing" & pct != 0 | status == "Present" & pct == 0) |>
    dplyr::mutate(
      variable_name_label = paste0(variable_name, " (", round(digits = 1, pct), "%)")
    ) |>
    dplyr::select(variable_name, variable_name_label, pct)

  tile_dat <- tile_dat |>
    dplyr::left_join(pct_var_df, dplyr::join_by(variable_name)) |>
    dplyr::mutate(variable_name_label = forcats::fct_reorder(variable_name_label, pct))

  gg <- tile_dat |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(
        x = order,
        y = variable_name_label,
        fill = status
      ),
      height = .9,
      width = .8
    ) +
    ggplot2::scale_fill_manual(
      "Assessment",
      values = c(
        "Missing" = col_vec[1],
        "Present" = col_vec[2]
      ),
      labels = c(
        glue::glue("Missing ({filter(pct_df, status=='Missing')$pct}%)"),
        glue::glue("Present ({filter(pct_df, status=='Present')$pct}%)")
      )
    ) +
    ggplot2::labs(
      y = "Variable Name",
      x = "Observations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.clip = "off",
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = y_axis_text_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (is.null(facet)) {
    gg
  } else {
    gg +
      ggplot2::facet_grid(
        ~ .data[[facet]],
        scales = "free",
        space = "free"
      )
  }
}
